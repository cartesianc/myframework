module MyFramework.Runtime.Demand.Control
  ( controlDemandAction
  , controlRegistryForSession
  ) where

import MyFramework.Runtime.Branch
  ( BranchSnapshot )
import MyFramework.Runtime.Control
  ( ControlFailure (..)
  , ControlOutcome (..)
  , ControlRegistry (..)
  , ControlResult (..)
  , DemandInvocation (..)
  )
import MyFramework.Runtime.Demand
  ( DemandSession
  , demandSessionBootRunId
  , demandInSession
  , forkDemandSessionFromAuthorized
  , snapshotDemandSession
  )
import MyFramework.Runtime.Types
  ( mintExecutionPermit
  )

-- | Replace only Control's demand callback. All other control behavior
-- remains in the caller-provided runtime registry.
controlRegistryForSession ::
  DemandSession ->
  ControlRegistry ->
  ControlRegistry
controlRegistryForSession currentSession currentRegistry =
  currentRegistry
    { controlDemandCallback =
        controlDemandAction currentSession
    }

-- | Every ControlAction evaluates from its supplied branch snapshot. Forks
-- isolate RuntimeState/ObservationStore and share only the boot HandleId
-- coordinator plus immutable Handler/operator registries.
controlDemandAction ::
  DemandSession ->
  DemandInvocation ->
  BranchSnapshot ->
  IO ControlResult
controlDemandAction currentSession currentInvocation currentSnapshot = do
  let currentPermit =
        mintExecutionPermit
          (demandSessionBootRunId currentSession)
          (demandInvocationPath currentInvocation)
          (demandInvocationNode currentInvocation)
  currentBranch <-
    forkDemandSessionFromAuthorized
      currentPermit
      currentSession
      currentSnapshot
  currentOutcome <-
    demandInSession
      currentBranch
      (demandInvocationNode currentInvocation)
  nextSnapshot <-
    snapshotDemandSession currentBranch
  pure
    ControlResult
      { controlResultSnapshot = nextSnapshot
      , controlResultOutcome =
          case currentOutcome of
            Right () ->
              ControlSucceeded
            Left currentFailure ->
              ControlFailed
                [ ControlRejected
                    (demandInvocationPath currentInvocation)
                    currentFailure
                ]
      , controlResultDiagnostics = []
      }
