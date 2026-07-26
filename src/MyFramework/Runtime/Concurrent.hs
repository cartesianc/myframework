{-# LANGUAGE RankNTypes #-}

module MyFramework.Runtime.Concurrent
  ( WorkerOutcome (..)
  , WorkerRace (..)
  , WorkerReport (..)
  , runParallelWorkers
  , runRaceWorkers
  , workerOutcomeSucceeded
  ) where

import Control.Concurrent
  ( MVar
  , ThreadId
  , forkIO
  , killThread
  , modifyMVar
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryPutMVar
  , tryReadMVar
  )
import Control.Exception
  ( SomeException
  , evaluate
  , mask
  , onException
  , try
  , uninterruptibleMask_
  )
import Control.Monad
  ( void )
import Data.List
  ( sortOn )
import qualified Data.Set as Set

data WorkerOutcome result
  = WorkerReturned result
  | WorkerRaised SomeException

instance Show result => Show (WorkerOutcome result) where
  show currentOutcome =
    case currentOutcome of
      WorkerReturned currentResult ->
        "WorkerReturned " ++ show currentResult
      WorkerRaised currentException ->
        "WorkerRaised " ++ show currentException

data WorkerReport result = WorkerReport
  { workerReportIndex :: Int
  , workerReportCompletionOrder :: Int
  , workerReportOutcome :: WorkerOutcome result
  }

instance Show result => Show (WorkerReport result) where
  show currentReport =
    "WorkerReport {workerReportIndex = "
      ++ show (workerReportIndex currentReport)
      ++ ", workerReportCompletionOrder = "
      ++ show (workerReportCompletionOrder currentReport)
      ++ ", workerReportOutcome = "
      ++ show (workerReportOutcome currentReport)
      ++ "}"

data WorkerRace result
  = WorkerRaceExhausted [WorkerReport result]
  | WorkerRaceWon
      Int
      [Int]
      [WorkerReport result]
  deriving (Show)

data Worker result = Worker
  { workerIndex :: Int
  , workerThread :: ThreadId
  , workerCompletion :: MVar (WorkerReport result)
  }

data WorkerGroup result = WorkerGroup
  { workerGroupWorkers :: [Worker result]
  , workerGroupSignal :: MVar ()
  }

runParallelWorkers :: [IO result] -> IO [WorkerReport result]
runParallelWorkers currentActions =
  mask
    ( \restore -> do
        currentGroup <-
          spawnWorkerGroup restore currentActions
        restore (awaitAllWorkers currentGroup)
          `onException` cancelAndDrainWorkers currentGroup
    )

runRaceWorkers ::
  (result -> Bool) ->
  [IO result] ->
  IO (WorkerRace result)
runRaceWorkers isSuccessful currentActions =
  mask
    ( \restore -> do
        currentGroup <-
          spawnWorkerGroup restore currentActions
        restore (selectFirstSuccess isSuccessful currentGroup)
          `onException` cancelAndDrainWorkers currentGroup
    )

workerOutcomeSucceeded ::
  (result -> Bool) ->
  WorkerOutcome result ->
  Bool
workerOutcomeSucceeded isSuccessful currentOutcome =
  case currentOutcome of
    WorkerReturned currentResult ->
      isSuccessful currentResult
    WorkerRaised _ ->
      False

spawnWorkerGroup ::
  (forall value. IO value -> IO value) ->
  [IO result] ->
  IO (WorkerGroup result)
spawnWorkerGroup restore currentActions = do
  currentSignal <- newEmptyMVar
  currentOrder <- newMVar 0
  currentWorkers <-
    spawnAll
      restore
      currentSignal
      currentOrder
      []
      (zip [0 ..] currentActions)
  pure
    WorkerGroup
      { workerGroupWorkers = currentWorkers
      , workerGroupSignal = currentSignal
      }

spawnAll ::
  (forall value. IO value -> IO value) ->
  MVar () ->
  MVar Int ->
  [Worker result] ->
  [(Int, IO result)] ->
  IO [Worker result]
spawnAll _ _ _ currentWorkers [] =
  pure (reverse currentWorkers)
spawnAll restore currentSignal currentOrder currentWorkers
  ((currentIndex, currentAction) : remaining) =
    ( do
        currentCompletion <- newEmptyMVar
        currentThread <-
          forkIO
            ( workerMain
                restore
                currentSignal
                currentOrder
                currentCompletion
                currentIndex
                currentAction
            )
        let currentWorker =
              Worker
                { workerIndex = currentIndex
                , workerThread = currentThread
                , workerCompletion = currentCompletion
                }
        spawnAll
          restore
          currentSignal
          currentOrder
          (currentWorker : currentWorkers)
          remaining
    )
      `onException` cancelAndDrainList currentWorkers

workerMain ::
  (forall value. IO value -> IO value) ->
  MVar () ->
  MVar Int ->
  MVar (WorkerReport result) ->
  Int ->
  IO result ->
  IO ()
workerMain restore currentSignal currentOrder currentCompletion currentIndex currentAction =
  mask
    ( \_ -> do
        currentOutcome <-
          try (restore currentAction >>= evaluate)
        publishWorkerReport
          currentSignal
          currentOrder
          currentCompletion
          currentIndex
          currentOutcome
    )

publishWorkerReport ::
  MVar () ->
  MVar Int ->
  MVar (WorkerReport result) ->
  Int ->
  Either SomeException result ->
  IO ()
publishWorkerReport currentSignal currentOrder currentCompletion currentIndex currentOutcome =
  -- A worker owns its empty completion cell. This bounded masked publish means
  -- ThreadKilled cannot leave a race/parallel waiter permanently blocked.
  uninterruptibleMask_ $ do
    nextOrder <-
      modifyMVar
        currentOrder
        ( \nextValue ->
            pure (nextValue + 1, nextValue)
        )
    putMVar
      currentCompletion
      WorkerReport
        { workerReportIndex = currentIndex
        , workerReportCompletionOrder = nextOrder
        , workerReportOutcome =
            case currentOutcome of
              Left currentException ->
                WorkerRaised currentException
              Right currentResult ->
                WorkerReturned currentResult
        }
    void (tryPutMVar currentSignal ())

awaitAllWorkers :: WorkerGroup result -> IO [WorkerReport result]
awaitAllWorkers currentGroup =
  traverse
    (readMVar . workerCompletion)
    (workerGroupWorkers currentGroup)

selectFirstSuccess ::
  (result -> Bool) ->
  WorkerGroup result ->
  IO (WorkerRace result)
selectFirstSuccess isSuccessful currentGroup
  | null currentWorkers =
      pure (WorkerRaceExhausted [])
  | otherwise =
      waitForWinner Set.empty
  where
    currentWorkers =
      workerGroupWorkers currentGroup
    allIndices =
      map workerIndex currentWorkers

    waitForWinner currentSeen
      | Set.size currentSeen == length currentWorkers =
          WorkerRaceExhausted
            <$> awaitAllWorkers currentGroup
      | otherwise = do
          takeMVar (workerGroupSignal currentGroup)
          currentReports <-
            completedReportsNotIn currentSeen currentWorkers
          let orderedReports =
                sortOn workerReportCompletionOrder currentReports
              nextSeen =
                foldr
                  (Set.insert . workerReportIndex)
                  currentSeen
                  orderedReports
          case firstSuccessful isSuccessful orderedReports of
            Nothing ->
              waitForWinner nextSeen
            Just currentWinner -> do
              let winnerOrder =
                    workerReportCompletionOrder currentWinner
                  settledBeforeWinner =
                    currentSeen
                      `Set.union` Set.fromList
                        [ workerReportIndex currentReport
                        | currentReport <- orderedReports
                        , workerReportCompletionOrder currentReport
                            < winnerOrder
                        ]
                  cancellationIndices =
                    [ currentIndex
                    | currentIndex <- allIndices
                    , currentIndex /= workerReportIndex currentWinner
                    , currentIndex
                        `Set.notMember` settledBeforeWinner
                    ]
              requestCancellation
                cancellationIndices
                currentWorkers
              finalReports <- awaitAllWorkers currentGroup
              pure
                ( WorkerRaceWon
                    (workerReportIndex currentWinner)
                    cancellationIndices
                    finalReports
                )

completedReportsNotIn ::
  Set.Set Int ->
  [Worker result] ->
  IO [WorkerReport result]
completedReportsNotIn currentSeen =
  collectCompleted []
  where
    collectCompleted currentReports [] =
      pure (reverse currentReports)
    collectCompleted currentReports (currentWorker : remaining)
      | workerIndex currentWorker `Set.member` currentSeen =
          collectCompleted currentReports remaining
      | otherwise = do
          currentReport <-
            tryReadMVar (workerCompletion currentWorker)
          case currentReport of
            Nothing ->
              collectCompleted currentReports remaining
            Just nextReport ->
              collectCompleted
                (nextReport : currentReports)
                remaining

firstSuccessful ::
  (result -> Bool) ->
  [WorkerReport result] ->
  Maybe (WorkerReport result)
firstSuccessful _ [] =
  Nothing
firstSuccessful isSuccessful (currentReport : remaining)
  | workerOutcomeSucceeded
      isSuccessful
      (workerReportOutcome currentReport) =
      Just currentReport
  | otherwise =
      firstSuccessful isSuccessful remaining

requestCancellation :: [Int] -> [Worker result] -> IO ()
requestCancellation currentIndices =
  mapM_
    ( \currentWorker ->
        if workerIndex currentWorker `elem` currentIndices
          then killThread (workerThread currentWorker)
          else pure ()
    )

cancelAndDrainWorkers :: WorkerGroup result -> IO ()
cancelAndDrainWorkers =
  cancelAndDrainList . workerGroupWorkers

cancelAndDrainList :: [Worker result] -> IO ()
cancelAndDrainList currentWorkers = do
  mapM_ (killThread . workerThread) currentWorkers
  mapM_ (readMVar . workerCompletion) currentWorkers
