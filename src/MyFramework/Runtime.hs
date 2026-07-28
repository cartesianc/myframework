module MyFramework.Runtime
  ( RuntimeProgram
  , RuntimePreparationError (..)
  , ControlOccurrenceExpectation (..)
  , ControlValidationError (..)
  , prepareRuntime
  , runtimeProgramLayout
  , RuntimeHooks (..)
  , RuntimeAction
  , runtimeAction
  , runRuntimeAction
  , succeedRuntimeAction
  , suspendRuntimeAction
  , failRuntimeAction
  , RuntimeGate (..)
  , RuntimeLoopPolicy (..)
  , RuntimeRun (..)
  , runRuntimeProgram
  , BranchSnapshot
  , StatusPlan (..)
  , ControlResult
  , ControlOutcome (..)
  , ControlFailure
  , ControlDiagnostic
  , controlResultSnapshot
  , controlResultOutcome
  , controlResultDiagnostics
  , controlResultSucceeded
  , DiagnosisResult
  , RExprEvaluationError (..)
  , BootRunId (..)
  , ExecutionProvenance (..)
  , CommitState (..)
  , FailurePhase (..)
  , RuntimeFailure (..)
  , ExecutionStatus (..)
  , ReadStatus (..)
  , Validity (..)
  , ImplementationStatus (..)
  , RuntimeData (..)
  , RuntimeEvent (..)
  , runtimeSnapshotExecutionStatus
  , runtimeSnapshotReadStatus
  , runtimeSnapshotValidity
  , runtimeSnapshotImplementationStatus
  , runtimeSnapshotReadValue
  , runtimeSnapshotEvents
  ) where

import MyFramework.Ast
  ( AstPath
  , MiddlewareRef
  )
import MyFramework.Ast.Layout
  ( AstBlueprintLayout
  , layoutAstBlueprint
  )
import MyFramework.Control
  ( ControlOccurrenceExpectation (..)
  , ControlPlan (..)
  , ControlValidationError (..)
  , StatusPlan (..)
  , compileControlPlan
  )
import MyFramework.CURDE.Core
  ( DemandGraph
  , ImplementationId
  , curdeCoreAst
  , curdeCoreDemandGraph
  )
import MyFramework.CURDE.Lowering
  ( LoweringResult (..)
  )
import MyFramework.CURDE.Validate
  ( ValidationError
  )
import MyFramework.CURDE.Types
  ( HandleId
  )
import MyFramework.Handler
  ( CommitState (..)
  , FailurePhase (..)
  , HandlerRegistry
  , RuntimeFailure (..)
  )
import MyFramework.Runtime.Branch
  ( BranchSnapshot (..)
  )
import MyFramework.Runtime.Control
  ( ControlAction
  , ControlDiagnostic
  , ControlFailure (..)
  , ControlGateResult (..)
  , ControlOutcome (..)
  , ControlRegistry (..)
  , ControlResult (..)
  , LoopPolicy (..)
  , controlResultSucceeded
  , runControlPlan
  )
import MyFramework.Runtime.Demand
  ( DemandSession
  , newDemandSession
  , snapshotDemandSession
  )
import MyFramework.Runtime.Demand.Control
  ( controlDemandAction
  )
import MyFramework.Runtime.Diagnosis
  ( DiagnosisResult
  , diagnoseFailedDemands
  )
import MyFramework.Runtime.Expression
  ( RExprEvaluationError (..)
  )
import MyFramework.Runtime.State
  ( RuntimeEvent (..)
  , executionStatusFor
  , implementationStatusFor
  , readStatusFor
  , readValueFor
  , runtimeEvents
  , validityFor
  )
import MyFramework.Runtime.Types
  ( BootRunId (..)
  , ExecutionProvenance (..)
  , ExecutionStatus (..)
  , ImplementationStatus (..)
  , ReadStatus (..)
  , Validity (..)
  , newBootRunId
  )
import MyFramework.Runtime.Value
  ( RuntimeData (..)
  )

-- | An opaque, validated executable projection. It is derived from the two
-- serializable configuration surfaces and contains no Handler implementation.
data RuntimeProgram = RuntimeProgram
  { programGraph :: DemandGraph
  , programControlPlan :: ControlPlan
  , runtimeProgramLayout :: AstBlueprintLayout
  }

-- | Lowering errors are rejected before Control compilation. Runtime never
-- executes a partial or best-effort Core.
data RuntimePreparationError
  = RuntimeLoweringRejected [ValidationError]
  | RuntimeControlRejected [ControlValidationError]
  deriving (Eq, Show)

prepareRuntime ::
  LoweringResult ->
  Either RuntimePreparationError RuntimeProgram
prepareRuntime currentLowering =
  case loweringErrors currentLowering of
    currentErrors@(_ : _) ->
      Left (RuntimeLoweringRejected currentErrors)
    [] ->
      case compileControlPlan currentCore of
        Left currentErrors ->
          Left (RuntimeControlRejected currentErrors)
        Right currentPlan ->
          Right
            RuntimeProgram
              { programGraph = curdeCoreDemandGraph currentCore
              , programControlPlan = currentPlan
              , runtimeProgramLayout =
                  layoutAstBlueprint (curdeCoreAst currentCore)
              }
  where
    currentCore =
      loweringCore currentLowering


-- | A wrapped Control action keeps the public hook API from exposing or
-- replacing the runtime's demand callback.
newtype RuntimeAction = RuntimeAction
  { unRuntimeAction :: ControlAction
  }

runtimeAction :: (BranchSnapshot -> IO ControlResult) -> RuntimeAction
runtimeAction =
  RuntimeAction

runRuntimeAction ::
  RuntimeAction ->
  BranchSnapshot ->
  IO ControlResult
runRuntimeAction =
  unRuntimeAction

succeedRuntimeAction :: RuntimeAction
succeedRuntimeAction =
  RuntimeAction
    ( \currentSnapshot ->
        pure
          ControlResult
            { controlResultSnapshot = currentSnapshot
            , controlResultOutcome = ControlSucceeded
            , controlResultDiagnostics = []
            }
    )

suspendRuntimeAction :: RuntimeAction
suspendRuntimeAction =
  RuntimeAction
    ( \currentSnapshot ->
        pure
          ControlResult
            { controlResultSnapshot = currentSnapshot
            , controlResultOutcome = ControlSuspended
            , controlResultDiagnostics = []
            }
    )

failRuntimeAction :: AstPath -> RuntimeFailure -> RuntimeAction
failRuntimeAction currentPath currentFailure =
  RuntimeAction
    ( \currentSnapshot ->
        pure
          ControlResult
            { controlResultSnapshot = currentSnapshot
            , controlResultOutcome =
                ControlFailed
                  [ControlRejected currentPath currentFailure]
            , controlResultDiagnostics = []
            }
    )

data RuntimeGate
  = RuntimeGateReady BranchSnapshot
  | RuntimeGateBlocked BranchSnapshot
  | RuntimeGateFailed BranchSnapshot RuntimeFailure
  deriving (Eq, Show)

data RuntimeLoopPolicy = RuntimeLoopPolicy
  { runtimeLoopIterationLimit :: Int
  , runtimeLoopStabilityPredicate ::
      BranchSnapshot ->
      BranchSnapshot ->
      Bool
  }

-- | Runtime-only control implementations. No defaults are supplied: an
-- explicit control node cannot silently degrade into a no-op.
data RuntimeHooks = RuntimeHooks
  { runtimeHookWait ::
      AstPath ->
      StatusPlan ->
      BranchSnapshot ->
      IO RuntimeGate
  , runtimeHookSuspense ::
      AstPath ->
      HandleId ->
      BranchSnapshot ->
      IO RuntimeGate
  , runtimeHookMiddleware ::
      MiddlewareRef ->
      AstPath ->
      RuntimeAction ->
      RuntimeAction
  , runtimeHookCallback ::
      HandleId ->
      AstPath ->
      RuntimeAction ->
      RuntimeAction
  , runtimeHookLoop ::
      AstPath ->
      RuntimeLoopPolicy
  }

data RuntimeRun = RuntimeRun
  { runtimeRunSnapshot :: BranchSnapshot
  , runtimeRunControlResult :: ControlResult
  , runtimeRunDiagnosisResult :: DiagnosisResult
  }
  deriving (Eq, Show)

-- | Run the single compiled boot tree. Every call creates a fresh coordinator.
-- Diagnosis is computed as a pure overlay from the final immutable snapshot.
runRuntimeProgram ::
  RuntimeProgram ->
  HandlerRegistry ->
  RuntimeHooks ->
  IO RuntimeRun
runRuntimeProgram currentProgram currentHandlers currentHooks = do
  currentBootRun <- newBootRunId
  currentSession <-
    newDemandSession
      currentBootRun
      currentHandlers
      (programGraph currentProgram)
  initialSnapshot <-
    snapshotDemandSession currentSession
  currentControl <-
    runControlPlan
      (controlRegistry currentSession currentHooks)
      (programControlPlan currentProgram)
      initialSnapshot
  let finalSnapshot =
        controlResultSnapshot currentControl
      currentDiagnosis =
        diagnoseFailedDemands
          (programGraph currentProgram)
          (programControlPlan currentProgram)
          (branchRuntimeState finalSnapshot)
  pure
    RuntimeRun
      { runtimeRunSnapshot = finalSnapshot
      , runtimeRunControlResult = currentControl
      , runtimeRunDiagnosisResult = currentDiagnosis
      }

controlRegistry ::
  DemandSession ->
  RuntimeHooks ->
  ControlRegistry
controlRegistry currentSession currentHooks =
  ControlRegistry
    { controlDemandCallback =
        controlDemandAction currentSession
    , controlWaitCallback =
        \currentPath currentStatus currentSnapshot ->
          toControlGate currentPath
            <$> runtimeHookWait
              currentHooks
              currentPath
              currentStatus
              currentSnapshot
    , controlSuspenseCallback =
        \currentPath currentHandle currentSnapshot ->
          toControlGate currentPath
            <$> runtimeHookSuspense
              currentHooks
              currentPath
              currentHandle
              currentSnapshot
    , controlMiddlewareCallback =
        \currentRef currentPath currentChild ->
          runRuntimeAction
            ( runtimeHookMiddleware
                currentHooks
                currentRef
                currentPath
                (runtimeAction currentChild)
            )
    , controlCallbackCallback =
        \currentHandle currentPath currentChild ->
          runRuntimeAction
            ( runtimeHookCallback
                currentHooks
                currentHandle
                currentPath
                (runtimeAction currentChild)
            )
    , controlLoopPolicy =
        \currentPath ->
          let currentPolicy =
                runtimeHookLoop currentHooks currentPath
           in LoopPolicy
                { loopIterationLimit =
                    runtimeLoopIterationLimit currentPolicy
                , loopStabilityPredicate =
                    runtimeLoopStabilityPredicate currentPolicy
                }
    }

toControlGate :: AstPath -> RuntimeGate -> ControlGateResult
toControlGate currentPath currentGate =
  case currentGate of
    RuntimeGateReady currentSnapshot ->
      ControlGateReady currentSnapshot
    RuntimeGateBlocked currentSnapshot ->
      ControlGateBlocked currentSnapshot
    RuntimeGateFailed currentSnapshot currentFailure ->
      ControlGateFailed
        currentSnapshot
        (ControlRejected currentPath currentFailure)

runtimeSnapshotExecutionStatus ::
  HandleId ->
  BranchSnapshot ->
  ExecutionStatus
runtimeSnapshotExecutionStatus currentId =
  executionStatusFor currentId . branchRuntimeState

runtimeSnapshotReadStatus ::
  HandleId ->
  BranchSnapshot ->
  ReadStatus
runtimeSnapshotReadStatus currentId =
  readStatusFor currentId . branchRuntimeState

runtimeSnapshotValidity ::
  HandleId ->
  BranchSnapshot ->
  Validity
runtimeSnapshotValidity currentId =
  validityFor currentId . branchRuntimeState

runtimeSnapshotImplementationStatus ::
  ImplementationId ->
  BranchSnapshot ->
  ImplementationStatus
runtimeSnapshotImplementationStatus currentId =
  implementationStatusFor currentId . branchRuntimeState

runtimeSnapshotReadValue ::
  HandleId ->
  BranchSnapshot ->
  Maybe RuntimeData
runtimeSnapshotReadValue currentId =
  readValueFor currentId . branchRuntimeState

runtimeSnapshotEvents :: BranchSnapshot -> [RuntimeEvent]
runtimeSnapshotEvents =
  runtimeEvents . branchRuntimeState
