{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception
  ( SomeException
  , displayException
  , try
  )
import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.List
  ( intercalate
  , sortOn
  )
import Numeric
  ( showHex )
import System.Exit
  ( exitFailure )

import MyFramework.Ast
import MyFramework.CURDE
import MyFramework.Handler
import MyFramework.Runtime
import MyFramework.Runtime.Diagnosis

data Check = Check
  { checkName :: String
  , checkPassed :: Bool
  , checkExpected :: String
  , checkActual :: String
  }

main :: IO ()
main = do
  currentResult <- try runAllScenarios
  case currentResult of
    Left currentException -> do
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "curde-runtime-witness.v1")
            , ("artifact", jsonString "curde-runtime-witness")
            , ("result", jsonString "failed")
            , ("fatal", jsonString (displayException (currentException :: SomeException)))
            ]
        )
      exitFailure
    Right currentChecks -> do
      let orderedChecks =
            sortOn checkName currentChecks
          succeeded =
            all checkPassed orderedChecks
      putStrLn
        ( jsonObject
            [ ("schema", jsonString "curde-runtime-witness.v1")
            , ("artifact", jsonString "curde-runtime-witness")
            , ("result", jsonString (if succeeded then "passed" else "failed"))
            , ( "claims"
              , jsonArray
                  [ renderRuntimeClaim
                      succeeded
                      "curde-runtime-observation-read-history"
                  , renderRuntimeClaim
                      succeeded
                      "curde-runtime-control-parity"
                  ]
              )
            , ("checks", jsonArray (map renderCheck orderedChecks))
            ]
        )
      unless succeeded exitFailure

runAllScenarios :: IO [Check]
runAllScenarios = do
  goodChecks <- runObservationSuccess
  failureChecks <- runObservationFailure
  parallelChecks <- runParallelSingleFlight
  safeFallbackChecks <- runSafeFallback
  unsafeFallbackChecks <- runUnsafeFallback
  controlChecks <- runControlAndHanging
  raceChecks <- runRaceSettlement
  pure
    ( goodChecks
        ++ failureChecks
        ++ parallelChecks
        ++ safeFallbackChecks
        ++ unsafeFallbackChecks
        ++ controlChecks
        ++ raceChecks
    )

integerSchema :: SchemaRef Integer
integerSchema =
  scalarSchema "Integer"

integerCodec :: ValueCodec Integer
integerCodec =
  ValueCodec
    { valueCodecSchema = integerSchema
    , encodeRuntimeData = Right . RuntimeInteger
    , decodeRuntimeData =
        \case
          RuntimeInteger currentValue ->
            Right currentValue
          currentValue ->
            Left
              ( RuntimeCodecDecodeFailed
                  (schemaRefIdentityOf integerSchema)
                  ("expected RuntimeInteger, observed " ++ show currentValue)
              )
    }

failingObservationCodec :: ValueCodec Integer
failingObservationCodec =
  integerCodec
    { encodeRuntimeData =
        const
          ( Left
              ( RuntimeCodecDecodeFailed
                  (schemaRefIdentityOf integerSchema)
                  "observation serialization rejected"
              )
          )
    }

unitCommandSpec :: Maybe SomeHandleRef -> CommandSpec () NoObservation
unitCommandSpec currentInput =
  CommandSpec
    { commandArgumentSchema = unitSchema
    , commandObservation = DiscardObservation
    , commandInput = currentInput
    }

observedIntegerSpec :: CommandSpec () Integer
observedIntegerSpec =
  CommandSpec
    { commandArgumentSchema = unitSchema
    , commandObservation = CaptureObservation integerSchema
    , commandInput = Nothing
    }

runtimeFailure ::
  FailurePhase ->
  CommitState ->
  String ->
  RuntimeFailure
runtimeFailure currentPhase currentCommit currentMessage =
  RuntimeFailure
    { runtimeFailurePhase = currentPhase
    , runtimeFailureCommitState = currentCommit
    , runtimeFailureMessage = currentMessage
    }

systemWith ::
  EffectSystemName ->
  [SomeHandleRef] ->
  [SomeHandleRef] ->
  EffectSystem
systemWith currentName currentHandles currentExports =
  effectSystem
    currentName
    []
    currentHandles
    []
    currentExports

prepareProgram ::
  [EffectSystem] ->
  AstBlueprintSeed ->
  Either RuntimePreparationError RuntimeProgram
prepareProgram currentSystems currentBlueprint =
  prepareRuntime (lowerCURDE currentSystems currentBlueprint)

plainHooks :: PureOperatorRegistry -> RuntimeHooks
plainHooks currentOperators =
  RuntimeHooks
    { runtimeHookOperators = currentOperators
    , runtimeHookWait =
        \_ _ currentSnapshot ->
          pure (RuntimeGateReady currentSnapshot)
    , runtimeHookSuspense =
        \_ _ currentSnapshot ->
          pure (RuntimeGateReady currentSnapshot)
    , runtimeHookMiddleware =
        \_ _ currentAction ->
          currentAction
    , runtimeHookCallback =
        \_ _ currentAction ->
          currentAction
    , runtimeHookContext =
        \_ _ currentAction ->
          currentAction
    , runtimeHookLoop =
        \_ ->
          RuntimeLoopPolicy
            { runtimeLoopIterationLimit = 2
            , runtimeLoopStabilityPredicate = \_ _ -> True
            }
    }

runObservationSuccess :: IO [Check]
runObservationSuccess = do
  sourceCount <- newIORef 0
  targetArguments <- newIORef []
  let currentName =
        EffectSystemName "runtime-good"
      sourceHandle =
        c @"goodSource" currentName observedIntegerSpec
      readHandle =
        r @"goodRead"
          currentName
          ReadSpec
            { readResultSchema = integerSchema
            , readInput = Just (SomeHandleRef sourceHandle)
            , readSource = ReadFromInputObservation
            }
      targetHandle =
        u @"goodTarget"
          currentName
          CommandSpec
            { commandArgumentSchema = integerSchema
            , commandObservation = DiscardObservation
            , commandInput = Just (SomeHandleRef sourceHandle)
            }
      incrementRef =
        OperatorRef "increment"
      currentImplementation =
        eraseImplementation
          ( implU
              targetHandle
              ( applyOperator
                  incrementRef
                  integerSchema
                  [SomeRExpr (rRef readHandle)]
              )
          )
      currentSystem =
        systemWith
          currentName
          [ SomeHandleRef sourceHandle
          , SomeHandleRef readHandle
          , SomeHandleRef targetHandle
          ]
          [SomeHandleRef targetHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedLeaf (ImplementationTarget currentImplementation)
          , astBlueprintSeedHanging = []
          }
      currentOperators =
        pureOperatorRegistry
          [ ( incrementRef
            , PureOperator
                { pureOperatorInputSchemas =
                    [schemaRefIdentityOf integerSchema]
                , pureOperatorOutputSchema =
                    schemaRefIdentityOf integerSchema
                , runPureOperator =
                    \case
                      [RuntimeInteger currentValue] ->
                        Right (RuntimeInteger (currentValue + 1))
                      currentValues ->
                        Left ("unexpected operator input " ++ show currentValues)
                }
            )
          ]
      currentRegistry = do
        withSource <-
          registerC
            sourceHandle
            ( commandHandler
                unitValueCodec
                integerCodec
                ( \_ () -> do
                    modifyIORef' sourceCount (+ 1)
                    pure (Right 41)
                )
            )
            emptyHandlerRegistry
        withRead <-
          registerR
            readHandle
            (readCodec integerCodec)
            withSource
        registerU
          targetHandle
          ( discardingCommandHandler
              integerCodec
              ( \currentInput currentArgument -> do
                  modifyIORef' targetArguments (++ [currentArgument])
                  pure
                    ( if handlerUses
                          (handleId sourceHandle)
                          currentInput
                        then Right ()
                        else
                          Left
                            ( runtimeFailure
                                DependencyPhase
                                NoExternalCommit
                                "goodTarget did not receive its declared input"
                            )
                    )
              )
          )
          withRead
  case (prepareProgram [currentSystem] currentBlueprint, currentOperators, currentRegistry) of
    (Right currentProgram, Right operators, Right handlers) -> do
      currentRun <-
        runRuntimeProgram
          currentProgram
          handlers
          (plainHooks operators)
      currentCount <- readIORef sourceCount
      currentArguments <- readIORef targetArguments
      let currentSnapshot =
            runtimeRunSnapshot currentRun
      pure
        [ boolCheck
            "observation.good-control-succeeded"
            True
            (controlResultSucceeded (runtimeRunControlResult currentRun))
        , eqCheck
            "observation.good-source-count-one"
            (1 :: Int)
            currentCount
        , eqCheck
            "observation.good-source-succeeded"
            ExecutionSucceeded
            (runtimeSnapshotExecutionStatus (handleId sourceHandle) currentSnapshot)
        , eqCheck
            "observation.good-read-available"
            ReadAvailable
            (runtimeSnapshotReadStatus (handleId readHandle) currentSnapshot)
        , eqCheck
            "observation.good-read-value-41"
            (Just (RuntimeInteger 41))
            (runtimeSnapshotReadValue (handleId readHandle) currentSnapshot)
        , eqCheck
            "observation.good-implementation-arg-42"
            [42]
            currentArguments
        , eqCheck
            "observation.good-target-succeeded"
            ExecutionSucceeded
            (runtimeSnapshotExecutionStatus (handleId targetHandle) currentSnapshot)
        , eqCheck
            "observation.good-implementation-succeeded"
            ImplementationSucceeded
            ( runtimeSnapshotImplementationStatus
                (implementationIdFor currentImplementation)
                currentSnapshot
            )
        ]
    currentFailure ->
      pure
        [failedCheck "observation.good-prepare" "prepared runtime, operator registry, and handler registry" (showPreparation currentFailure)]

runObservationFailure :: IO [Check]
runObservationFailure = do
  sourceCount <- newIORef 0
  targetCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-bad-observation"
      sourceHandle =
        c @"badSource" currentName observedIntegerSpec
      readHandle =
        r @"badRead"
          currentName
          ReadSpec
            { readResultSchema = integerSchema
            , readInput = Just (SomeHandleRef sourceHandle)
            , readSource = ReadFromInputObservation
            }
      targetHandle =
        u @"badTarget"
          currentName
          CommandSpec
            { commandArgumentSchema = integerSchema
            , commandObservation = DiscardObservation
            , commandInput = Just (SomeHandleRef sourceHandle)
            }
      currentImplementation =
        eraseImplementation
          (implU targetHandle (rRef readHandle))
      currentSystem =
        systemWith
          currentName
          [ SomeHandleRef sourceHandle
          , SomeHandleRef readHandle
          , SomeHandleRef targetHandle
          ]
          [SomeHandleRef targetHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedLeaf (ImplementationTarget currentImplementation)
          , astBlueprintSeedHanging = []
          }
      currentRegistry = do
        withSource <-
          registerC
            sourceHandle
            ( commandHandler
                unitValueCodec
                failingObservationCodec
                ( \_ () -> do
                    modifyIORef' sourceCount (+ 1)
                    pure (Right 41)
                )
            )
            emptyHandlerRegistry
        withRead <-
          registerR
            readHandle
            (readCodec integerCodec)
            withSource
        registerU
          targetHandle
          ( discardingCommandHandler
              integerCodec
              ( \_ _ -> do
                  modifyIORef' targetCount (+ 1)
                  pure (Right ())
              )
          )
          withRead
  case (prepareProgram [currentSystem] currentBlueprint, currentRegistry) of
    (Right currentProgram, Right handlers) -> do
      currentRun <-
        runRuntimeProgram
          currentProgram
          handlers
          (plainHooks emptyPureOperatorRegistry)
      currentSourceCount <- readIORef sourceCount
      currentTargetCount <- readIORef targetCount
      let currentSnapshot =
            runtimeRunSnapshot currentRun
          currentDiagnosis =
            runtimeRunDiagnosisResult currentRun
          currentReports =
            diagnosisResultReports currentDiagnosis
          rootIsBadRead currentReport =
            diagnosisRootNode (diagnosisReportRoot currentReport)
              == HandleNode (handleId readHandle)
              && diagnosisRootChannel (diagnosisReportRoot currentReport)
                == ReadFailureChannel
          sourceIsSuspect currentImpact =
            diagnosisImpactKind currentImpact
              == DiagnosisSuspectImpact
              && diagnosisSnapshotNode
                (diagnosisImpactSnapshot currentImpact)
                == HandleNode (handleId sourceHandle)
          implementationIsPolluted currentImpact =
            diagnosisImpactKind currentImpact
              == DiagnosisPollutedImpact
              && diagnosisSnapshotNode
                (diagnosisImpactSnapshot currentImpact)
                == ImplementationNode
                  (implementationIdFor currentImplementation)
          pollutedHasPath currentImpact =
            implementationIsPolluted currentImpact
              && not (null (diagnosisImpactControlPaths currentImpact))
          allImpacts =
            concatMap diagnosisReportImpacts currentReports
      pure
        [ eqCheck
            "observation.bad-source-count-one"
            (1 :: Int)
            currentSourceCount
        , eqCheck
            "observation.bad-source-still-succeeded"
            ExecutionSucceeded
            (runtimeSnapshotExecutionStatus (handleId sourceHandle) currentSnapshot)
        , boolCheck
            "observation.bad-read-unavailable"
            True
            ( isReadUnavailable
                (runtimeSnapshotReadStatus (handleId readHandle) currentSnapshot)
            )
        , eqCheck
            "observation.bad-target-handler-not-called"
            (0 :: Int)
            currentTargetCount
        , boolCheck
            "observation.bad-control-failed"
            True
            ( not
                (controlResultSucceeded (runtimeRunControlResult currentRun))
            )
        , boolCheck
            "diagnosis.bad-read-root"
            True
            (any rootIsBadRead currentReports)
        , boolCheck
            "diagnosis.bad-source-suspect"
            True
            (any sourceIsSuspect allImpacts)
        , eqCheck
            "observation.bad-source-validity-suspect"
            Suspect
            (runtimeSnapshotValidity (handleId sourceHandle) currentSnapshot)
        , boolCheck
            "diagnosis.bad-implementation-polluted"
            True
            (any implementationIsPolluted allImpacts)
        , boolCheck
            "diagnosis.bad-occurrence-path"
            True
            (any pollutedHasPath allImpacts)
        ]
    currentFailure ->
      pure
        [failedCheck "observation.bad-prepare" "prepared runtime and handler registry" (showPreparedRegistry currentFailure)]

runParallelSingleFlight :: IO [Check]
runParallelSingleFlight = do
  sourceCount <- newIORef 0
  consumerUseCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-parallel"
      sourceHandle =
        c @"sharedRoot" currentName (unitCommandSpec Nothing)
      consumerA =
        e @"sharedConsumerA"
          currentName
          (unitCommandSpec (Just (SomeHandleRef sourceHandle)))
      consumerB =
        e @"sharedConsumerB"
          currentName
          (unitCommandSpec (Just (SomeHandleRef sourceHandle)))
      currentSystem =
        systemWith
          currentName
          [ SomeHandleRef sourceHandle
          , SomeHandleRef consumerA
          , SomeHandleRef consumerB
          ]
          [SomeHandleRef consumerA, SomeHandleRef consumerB]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedParallel
                [ SeedLeaf (HandleTarget (handleRefFor consumerA))
                , SeedLeaf (HandleTarget (handleRefFor consumerB))
                ]
          , astBlueprintSeedHanging = []
          }
      sourceHandler =
        discardingCommandHandler
          unitValueCodec
          ( \_ () -> do
              modifyIORef' sourceCount (+ 1)
              threadDelay 50000
              pure (Right ())
          )
      consumerHandler =
        discardingCommandHandler
          unitValueCodec
          ( \currentInput () -> do
              if handlerUses (handleId sourceHandle) currentInput
                then do
                  modifyIORef' consumerUseCount (+ 1)
                  pure (Right ())
                else
                  pure
                    ( Left
                        ( runtimeFailure
                            DependencyPhase
                            NoExternalCommit
                            "consumer did not use shared input status"
                        )
                    )
          )
      currentRegistry = do
        withSource <-
          registerC sourceHandle sourceHandler emptyHandlerRegistry
        withA <-
          registerE consumerA consumerHandler withSource
        registerE consumerB consumerHandler withA
  case (prepareProgram [currentSystem] currentBlueprint, currentRegistry) of
    (Right currentProgram, Right handlers) -> do
      currentRun <-
        runRuntimeProgram
          currentProgram
          handlers
          (plainHooks emptyPureOperatorRegistry)
      currentSourceCount <- readIORef sourceCount
      currentUseCount <- readIORef consumerUseCount
      let currentSnapshot =
            runtimeRunSnapshot currentRun
      pure
        [ boolCheck
            "parallel.control-succeeded"
            True
            (controlResultSucceeded (runtimeRunControlResult currentRun))
        , eqCheck
            "parallel.shared-action-single-flight"
            (1 :: Int)
            currentSourceCount
        , eqCheck
            "parallel.consumers-used-input"
            (2 :: Int)
            currentUseCount
        , eqCheck
            "parallel.consumer-a-succeeded"
            ExecutionSucceeded
            (runtimeSnapshotExecutionStatus (handleId consumerA) currentSnapshot)
        , eqCheck
            "parallel.consumer-b-succeeded"
            ExecutionSucceeded
            (runtimeSnapshotExecutionStatus (handleId consumerB) currentSnapshot)
        ]
    currentFailure ->
      pure
        [failedCheck "parallel.prepare" "prepared runtime and handler registry" (showPreparedRegistry currentFailure)]

runSafeFallback :: IO [Check]
runSafeFallback = do
  primaryCount <- newIORef 0
  fallbackCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-safe-fallback"
      primaryHandle =
        e @"safePrimary" currentName (unitCommandSpec Nothing)
      fallbackHandle =
        e @"safeFallback" currentName (unitCommandSpec Nothing)
      currentSystem =
        systemWith
          currentName
          [SomeHandleRef primaryHandle, SomeHandleRef fallbackHandle]
          [SomeHandleRef primaryHandle, SomeHandleRef fallbackHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedFallback
                [ SeedLeaf (HandleTarget (handleRefFor primaryHandle))
                , SeedLeaf (HandleTarget (handleRefFor fallbackHandle))
                ]
          , astBlueprintSeedHanging = []
          }
      currentRegistry = do
        withPrimary <-
          registerE
            primaryHandle
            ( discardingCommandHandler
                unitValueCodec
                ( \_ () -> do
                    modifyIORef' primaryCount (+ 1)
                    pure
                      ( Left
                          ( runtimeFailure
                              HandlerPhase
                              NoExternalCommit
                              "safe primary rejected"
                          )
                      )
                )
            )
            emptyHandlerRegistry
        registerE
          fallbackHandle
          ( discardingCommandHandler
              unitValueCodec
              ( \_ () -> do
                  modifyIORef' fallbackCount (+ 1)
                  pure (Right ())
              )
          )
          withPrimary
  runFallbackScenario
    "fallback.safe"
    currentSystem
    currentBlueprint
    currentRegistry
    primaryCount
    fallbackCount
    True
    1

runUnsafeFallback :: IO [Check]
runUnsafeFallback = do
  primaryCount <- newIORef 0
  fallbackCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-unsafe-fallback"
      primaryHandle =
        e @"unsafePrimary" currentName (unitCommandSpec Nothing)
      fallbackHandle =
        e @"forbiddenFallback" currentName (unitCommandSpec Nothing)
      currentSystem =
        systemWith
          currentName
          [SomeHandleRef primaryHandle, SomeHandleRef fallbackHandle]
          [SomeHandleRef primaryHandle, SomeHandleRef fallbackHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedFallback
                [ SeedLeaf (HandleTarget (handleRefFor primaryHandle))
                , SeedLeaf (HandleTarget (handleRefFor fallbackHandle))
                ]
          , astBlueprintSeedHanging = []
          }
      currentRegistry = do
        withPrimary <-
          registerE
            primaryHandle
            ( discardingCommandHandler
                unitValueCodec
                ( \_ () -> do
                    modifyIORef' primaryCount (+ 1)
                    pure
                      ( Left
                          ( runtimeFailure
                              HandlerPhase
                              ExternalCommitted
                              "unsafe primary failed after commit"
                          )
                      )
                )
            )
            emptyHandlerRegistry
        registerE
          fallbackHandle
          ( discardingCommandHandler
              unitValueCodec
              ( \_ () -> do
                  modifyIORef' fallbackCount (+ 1)
                  pure (Right ())
              )
          )
          withPrimary
  runFallbackScenario
    "fallback.unsafe"
    currentSystem
    currentBlueprint
    currentRegistry
    primaryCount
    fallbackCount
    False
    0

runFallbackScenario ::
  String ->
  EffectSystem ->
  AstBlueprintSeed ->
  Either RegistryError HandlerRegistry ->
  IORef Int ->
  IORef Int ->
  Bool ->
  Int ->
  IO [Check]
runFallbackScenario
  currentPrefix
  currentSystem
  currentBlueprint
  currentRegistry
  primaryCount
  fallbackCount
  expectedSuccess
  expectedFallbackCount =
    case (prepareProgram [currentSystem] currentBlueprint, currentRegistry) of
      (Right currentProgram, Right handlers) -> do
        currentRun <-
          runRuntimeProgram
            currentProgram
            handlers
            (plainHooks emptyPureOperatorRegistry)
        currentPrimaryCount <- readIORef primaryCount
        currentFallbackCount <- readIORef fallbackCount
        pure
          [ eqCheck
              (currentPrefix ++ ".primary-count-one")
              (1 :: Int)
              currentPrimaryCount
          , eqCheck
              (currentPrefix ++ ".fallback-count")
              expectedFallbackCount
              currentFallbackCount
          , eqCheck
              (currentPrefix ++ ".control-success")
              expectedSuccess
              (controlResultSucceeded (runtimeRunControlResult currentRun))
          ]
      currentFailure ->
        pure
          [failedCheck (currentPrefix ++ ".prepare") "prepared runtime and handler registry" (showPreparedRegistry currentFailure)]

runControlAndHanging :: IO [Check]
runControlAndHanging = do
  workCount <- newIORef 0
  hangingCount <- newIORef 0
  waitCount <- newIORef 0
  suspenseCount <- newIORef 0
  middlewareBeforeCount <- newIORef 0
  middlewareAfterCount <- newIORef 0
  callbackCount <- newIORef 0
  contextCount <- newIORef 0
  loopPolicyCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-control"
      workHandle =
        e @"controlWork" currentName (unitCommandSpec Nothing)
      hangingHandle =
        e @"hangingWork" currentName (unitCommandSpec Nothing)
      selectedKey =
        ChoiceKey "selected"
      currentSystem =
        systemWith
          currentName
          [SomeHandleRef workHandle, SomeHandleRef hangingHandle]
          [SomeHandleRef workHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedContext
                (ContextRef "runtime-context")
                ( SeedMiddleware
                    (MiddlewareRef "runtime-middleware")
                    ( SeedCallback
                        (handleRefFor workHandle)
                        ( SeedWait
                            (StatusOf (handleRefFor workHandle))
                            ( SeedChain
                                [ SeedLoop
                                    ( SeedLeaf
                                        (HandleTarget (handleRefFor workHandle))
                                    )
                                , SeedChoice
                                    selectedKey
                                    [ ( selectedKey
                                      , SeedSuspense (handleRefFor workHandle)
                                      )
                                    ]
                                ]
                            )
                        )
                    )
                )
          , astBlueprintSeedHanging =
              [SeedLeaf (HandleTarget (handleRefFor hangingHandle))]
          }
      currentRegistry = do
        withWork <-
          registerE
            workHandle
            ( discardingCommandHandler
                unitValueCodec
                ( \_ () -> do
                    modifyIORef' workCount (+ 1)
                    pure (Right ())
                )
            )
            emptyHandlerRegistry
        registerE
          hangingHandle
          ( discardingCommandHandler
              unitValueCodec
              ( \_ () -> do
                  modifyIORef' hangingCount (+ 1)
                  pure (Right ())
              )
          )
          withWork
      currentHooks =
        RuntimeHooks
          { runtimeHookOperators = emptyPureOperatorRegistry
          , runtimeHookWait =
              \_ _ currentSnapshot -> do
                modifyIORef' waitCount (+ 1)
                pure (RuntimeGateReady currentSnapshot)
          , runtimeHookSuspense =
              \_ _ currentSnapshot -> do
                modifyIORef' suspenseCount (+ 1)
                pure (RuntimeGateReady currentSnapshot)
          , runtimeHookMiddleware =
              \_ _ ->
                trackedWrapper
                  middlewareBeforeCount
                  middlewareAfterCount
          , runtimeHookCallback =
              \_ _ ->
                beforeWrapper callbackCount
          , runtimeHookContext =
              \_ _ ->
                beforeWrapper contextCount
          , runtimeHookLoop =
              \_ ->
                RuntimeLoopPolicy
                  { runtimeLoopIterationLimit = 2
                  , runtimeLoopStabilityPredicate =
                      \_ _ ->
                        True
                  }
          }
  case (prepareProgram [currentSystem] currentBlueprint, currentRegistry) of
    (Right currentProgram, Right handlers) -> do
      modifyIORef' loopPolicyCount (+ 1)
      currentRun <-
        runRuntimeProgram
          currentProgram
          handlers
          currentHooks
      currentWorkCount <- readIORef workCount
      currentHangingCount <- readIORef hangingCount
      currentWaitCount <- readIORef waitCount
      currentSuspenseCount <- readIORef suspenseCount
      currentMiddlewareBefore <- readIORef middlewareBeforeCount
      currentMiddlewareAfter <- readIORef middlewareAfterCount
      currentCallbackCount <- readIORef callbackCount
      currentContextCount <- readIORef contextCount
      currentLoopCount <- readIORef loopPolicyCount
      let currentSnapshot =
            runtimeRunSnapshot currentRun
          hangingEvents =
            filter (eventNamesHandle (handleId hangingHandle))
              (runtimeSnapshotEvents currentSnapshot)
      pure
        [ boolCheck
            "control.composed-succeeded"
            True
            (controlResultSucceeded (runtimeRunControlResult currentRun))
        , eqCheck "control.work-count-one" (1 :: Int) currentWorkCount
        , eqCheck "control.wait-hook" (1 :: Int) currentWaitCount
        , eqCheck "control.suspense-hook" (1 :: Int) currentSuspenseCount
        , eqCheck "control.middleware-before-hook" (1 :: Int) currentMiddlewareBefore
        , eqCheck "control.middleware-after-hook" (1 :: Int) currentMiddlewareAfter
        , eqCheck "control.callback-hook" (1 :: Int) currentCallbackCount
        , eqCheck "control.context-hook" (1 :: Int) currentContextCount
        , eqCheck "control.loop-policy" (1 :: Int) currentLoopCount
        , eqCheck
            "hanging.declared-count"
            (1 :: Int)
            (runtimeProgramHangingCount currentProgram)
        , eqCheck "hanging.handler-not-called" (0 :: Int) currentHangingCount
        , eqCheck
            "hanging.status-unused"
            ExecutionUnused
            (runtimeSnapshotExecutionStatus (handleId hangingHandle) currentSnapshot)
        , eqCheck "hanging.no-runtime-event" (0 :: Int) (length hangingEvents)
        ]
    currentFailure ->
      pure
        [failedCheck "control.prepare" "prepared runtime and handler registry" (showPreparedRegistry currentFailure)]

runRaceSettlement :: IO [Check]
runRaceSettlement = do
  slowStarted <- newEmptyMVar
  slowBlocker <- newEmptyMVar
  slowCompleted <- newIORef 0
  fastCount <- newIORef 0
  let currentName =
        EffectSystemName "runtime-race"
      slowHandle =
        e @"slowGate" currentName (unitCommandSpec Nothing)
      fastHandle =
        e @"fastGate" currentName (unitCommandSpec Nothing)
      currentSystem =
        systemWith
          currentName
          [SomeHandleRef slowHandle, SomeHandleRef fastHandle]
          [SomeHandleRef slowHandle, SomeHandleRef fastHandle]
      currentBlueprint =
        AstBlueprintSeed
          { astBlueprintSeedBoot =
              SeedRace
                [ SeedSuspense (handleRefFor slowHandle)
                , SeedSuspense (handleRefFor fastHandle)
                ]
          , astBlueprintSeedHanging = []
          }
      currentHooks =
        (plainHooks emptyPureOperatorRegistry)
          { runtimeHookSuspense =
              \_ currentId currentSnapshot ->
                if currentId == handleId slowHandle
                  then do
                    putMVar slowStarted ()
                    _ <- takeMVar slowBlocker
                    modifyIORef' slowCompleted (+ 1)
                    pure (RuntimeGateReady currentSnapshot)
                  else do
                    readMVar slowStarted
                    modifyIORef' fastCount (+ 1)
                    pure (RuntimeGateReady currentSnapshot)
          }
  case prepareProgram [currentSystem] currentBlueprint of
    Right currentProgram -> do
      currentRun <-
        runRuntimeProgram
          currentProgram
          emptyHandlerRegistry
          currentHooks
      currentSlowCompleted <- readIORef slowCompleted
      currentFastCount <- readIORef fastCount
      pure
        [ boolCheck
            "race.uncertain-cancellation-rejected"
            False
            (controlResultSucceeded (runtimeRunControlResult currentRun))
        , eqCheck "race.fast-winner-count" (1 :: Int) currentFastCount
        , eqCheck "race.cancelled-branch-not-completed" (0 :: Int) currentSlowCompleted
        , eqCheck
            "race.no-runtime-state-events"
            (0 :: Int)
            ( length
                (runtimeSnapshotEvents (runtimeRunSnapshot currentRun))
            )
        ]
    Left currentFailure ->
      pure
        [failedCheck "race.prepare" "prepared runtime" (show currentFailure)]

trackedWrapper ::
  IORef Int ->
  IORef Int ->
  RuntimeAction ->
  RuntimeAction
trackedWrapper beforeCount afterCount currentAction =
  runtimeAction
    ( \currentSnapshot -> do
        modifyIORef' beforeCount (+ 1)
        currentResult <-
          runRuntimeAction currentAction currentSnapshot
        modifyIORef' afterCount (+ 1)
        pure currentResult
    )

beforeWrapper :: IORef Int -> RuntimeAction -> RuntimeAction
beforeWrapper currentCount currentAction =
  runtimeAction
    ( \currentSnapshot -> do
        modifyIORef' currentCount (+ 1)
        runRuntimeAction currentAction currentSnapshot
    )

handlerUses :: HandleId -> HandlerInput -> Bool
handlerUses expectedId currentInput =
  handlerInputHandleId currentInput == Just expectedId
    && handlerInputExecutionStatus currentInput == Just ExecutionSucceeded
    && handlerInputValidity currentInput == Just Trusted

isReadUnavailable :: ReadStatus -> Bool
isReadUnavailable currentStatus =
  case currentStatus of
    ReadObservationUnavailable _ ->
      True
    _ ->
      False

eventNamesHandle :: HandleId -> RuntimeEvent -> Bool
eventNamesHandle expectedId currentEvent =
  case currentEvent of
    ExecutionStatusChanged currentId _ ->
      currentId == expectedId
    ReadStatusChanged currentId _ ->
      currentId == expectedId
    ImplementationStatusChanged currentId _ ->
      implementationIdTarget currentId == expectedId
    ValidityChanged currentId _ ->
      currentId == expectedId
    ReadValueAvailable currentId ->
      currentId == expectedId
    RuntimeObservationCaptured currentId ->
      currentId == expectedId
    RuntimeObservationUnavailable currentId _ ->
      currentId == expectedId

boolCheck :: String -> Bool -> Bool -> Check
boolCheck currentName expectedValue actualValue =
  eqCheck currentName expectedValue actualValue

eqCheck :: (Eq value, Show value) => String -> value -> value -> Check
eqCheck currentName expectedValue actualValue =
  Check
    { checkName = currentName
    , checkPassed = expectedValue == actualValue
    , checkExpected = show expectedValue
    , checkActual = show actualValue
    }

failedCheck :: String -> String -> String -> Check
failedCheck currentName expectedValue actualValue =
  Check
    { checkName = currentName
    , checkPassed = False
    , checkExpected = expectedValue
    , checkActual = actualValue
    }

showPreparation ::
  ( Either RuntimePreparationError RuntimeProgram
  , Either RExprEvaluationError PureOperatorRegistry
  , Either RegistryError HandlerRegistry
  ) ->
  String
showPreparation (currentProgram, currentOperators, currentHandlers) =
  intercalate
    ";"
    [ either show (const "prepared") currentProgram
    , either show (const "operators-ready") currentOperators
    , either show (const "handlers-ready") currentHandlers
    ]

showPreparedRegistry ::
  ( Either RuntimePreparationError RuntimeProgram
  , Either RegistryError HandlerRegistry
  ) ->
  String
showPreparedRegistry (currentProgram, currentHandlers) =
  intercalate
    ";"
    [ either show (const "prepared") currentProgram
    , either show (const "handlers-ready") currentHandlers
    ]
renderRuntimeClaim :: Bool -> String -> String
renderRuntimeClaim succeeded currentName =
  jsonObject
    [ ("name", jsonString currentName)
    , ( "status"
      , jsonString
          (if succeeded then "established" else "violated")
      )
    ]

renderCheck :: Check -> String
renderCheck currentCheck =
  jsonObject
    [ ("name", jsonString (checkName currentCheck))
    , ("passed", jsonBool (checkPassed currentCheck))
    , ("expected", jsonString (checkExpected currentCheck))
    , ("actual", jsonString (checkActual currentCheck))
    ]

jsonObject :: [(String, String)] -> String
jsonObject currentFields =
  "{"
    ++ intercalate
      ","
      [jsonString currentName ++ ":" ++ currentValue | (currentName, currentValue) <- currentFields]
    ++ "}"

jsonArray :: [String] -> String
jsonArray currentValues =
  "[" ++ intercalate "," currentValues ++ "]"

jsonBool :: Bool -> String
jsonBool True = "true"
jsonBool False = "false"

jsonString :: String -> String
jsonString currentValue =
  "\"" ++ concatMap escapeJsonChar currentValue ++ "\""

escapeJsonChar :: Char -> String
escapeJsonChar currentChar =
  case currentChar of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _
      | ord currentChar < 0x20 ->
          "\\u" ++ padLeft 4 '0' (showHex (ord currentChar) "")
      | otherwise ->
          [currentChar]

padLeft :: Int -> Char -> String -> String
padLeft currentWidth currentFill currentValue =
  replicate (max 0 (currentWidth - length currentValue)) currentFill
    ++ currentValue
