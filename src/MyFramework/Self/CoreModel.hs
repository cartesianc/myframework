{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module MyFramework.Self.CoreModel
  ( EmptyBusiness (..)
  , FrameworkAsBusiness
  , FrameworkInterpretError (..)
  , FrameworkSemanticObservation (..)
  , candidateHasNoPreviousCoreDependency
  , decodeFrameworkAsBusiness
  , encodeFrameworkAsBusiness
  , frameworkAsBusiness
  , frameworkAsBusinessAstSeed
  , frameworkAsBusinessEffectSystems
  , frameworkAsBusinessHandlerCoverage
  , frameworkAsBusinessSchema
  , frameworkCloseEmptyHandle
  , frameworkCompileCataHandle
  , frameworkCoreAstBlueprint
  , frameworkCoreEffectSystemDeclarations
  , frameworkLowerCurdeHandle
  , frameworkValidateFacadeHandle
  , interpretFrameworkAsBusiness
  ) where

import Data.List
  ( intercalate )
import GHC.Generics
  ( Generic )
import Text.Read
  ( readMaybe )

import MyFramework.Ast
  ( AstBlueprintSeed (..)
  , AstSeed (..)
  , AstTarget (..)
  )
import MyFramework.CURDE
import MyFramework.Handler
import MyFramework.Runtime
import MyFramework.TrustBase.Core
  ( CoreId )
import MyFramework.TrustBase.Digest
  ( sha256 )

data FrameworkCoreHandles = FrameworkCoreHandles
  { validateFacade :: Handle "validateFacade" 'R () String
  , lowerCurde :: Handle "lowerCurde" 'R () String
  , compileCata :: Handle "compileCata" 'R () String
  , closeEmpty :: Handle "closeEmpty" 'E String NoObservation
  }
  deriving (Generic)

-- | The terminal App argument contains no CURDE declaration, AST node,
-- handler, implementation, runtime value, or host capability.
data EmptyBusiness = EmptyBusiness
  deriving (Eq, Ord, Read, Show)

data FrameworkAsBusiness = FrameworkAsBusiness
  { frameworkAsBusinessSchema :: String
  , frameworkAsBusinessEffectSystems :: [EffectSystemDecl]
  , frameworkAsBusinessAstSeed :: AstBlueprintSeed
  , frameworkAsBusinessHandlerCoverage :: [HandleId]
  , frameworkAsBusinessClaims :: [String]
  }
  deriving (Eq, Ord, Read, Show)

data FrameworkInterpretError
  = FrameworkHandlerRegistrationFailed RegistryError
  | FrameworkRuntimePreparationFailed RuntimePreparationError
  | FrameworkRuntimeDidNotClose String
  deriving (Eq, Show)

data FrameworkSemanticObservation = FrameworkSemanticObservation
  { frameworkObservationSemanticDigest :: String
  , frameworkObservationControlSucceeded :: Bool
  , frameworkObservationFinalExecutionStatus :: ExecutionStatus
  , frameworkObservationRuntimeDependencies :: [CoreId]
  }
  deriving (Eq, Show)

frameworkCoreSchemaV1 :: String
frameworkCoreSchemaV1 =
  "myframework-framework-as-business.v1"

frameworkCoreName :: EffectSystemName
frameworkCoreName =
  EffectSystemName "framework-core"

frameworkTokenSchema :: SchemaRef String
frameworkTokenSchema =
  scalarSchema "FrameworkSemanticToken"

frameworkTokenCodec :: ValueCodec String
frameworkTokenCodec =
  ValueCodec
    { valueCodecSchema = frameworkTokenSchema
    , encodeRuntimeData = Right . RuntimeText
    , decodeRuntimeData =
        \currentValue ->
          case currentValue of
            RuntimeText currentText ->
              Right currentText
            _ ->
              Left
                ( RuntimeCodecDecodeFailed
                    (schemaRefIdentityOf frameworkTokenSchema)
                    "expected RuntimeText framework token"
                )
    }

frameworkValidateFacadeHandle ::
  Handle "validateFacade" 'R () String
frameworkValidateFacadeHandle =
  r @"validateFacade"
    frameworkCoreName
    ReadSpec
      { readResultSchema = frameworkTokenSchema
      , readInput = Nothing
      , readSource = ReadFromHandler
      }

frameworkLowerCurdeHandle ::
  Handle "lowerCurde" 'R () String
frameworkLowerCurdeHandle =
  r @"lowerCurde"
    frameworkCoreName
    ReadSpec
      { readResultSchema = frameworkTokenSchema
      , readInput =
          Just (SomeHandleRef frameworkValidateFacadeHandle)
      , readSource = ReadFromHandler
      }

frameworkCompileCataHandle ::
  Handle "compileCata" 'R () String
frameworkCompileCataHandle =
  r @"compileCata"
    frameworkCoreName
    ReadSpec
      { readResultSchema = frameworkTokenSchema
      , readInput =
          Just (SomeHandleRef frameworkLowerCurdeHandle)
      , readSource = ReadFromHandler
      }

frameworkCloseEmptyHandle ::
  Handle "closeEmpty" 'E String NoObservation
frameworkCloseEmptyHandle =
  e @"closeEmpty"
    frameworkCoreName
    CommandSpec
      { commandArgumentSchema = frameworkTokenSchema
      , commandObservation = DiscardObservation
      , commandInput = Nothing
      }

frameworkCloseEmptyImplementation :: ImplementationDecl
frameworkCloseEmptyImplementation =
  eraseImplementation
    ( implE
        frameworkCloseEmptyHandle
        (rRef frameworkCompileCataHandle)
    )

frameworkCoreHandles :: FrameworkCoreHandles
frameworkCoreHandles =
  FrameworkCoreHandles
    { validateFacade = frameworkValidateFacadeHandle
    , lowerCurde = frameworkLowerCurdeHandle
    , compileCata = frameworkCompileCataHandle
    , closeEmpty = frameworkCloseEmptyHandle
    }

frameworkCoreEffectSystemDeclarations ::
  Either [RecordError] [EffectSystemDecl]
frameworkCoreEffectSystemDeclarations = do
  currentSystem <-
    effectSystemFromRecord
      frameworkCoreName
      []
      frameworkCoreHandles
      []
      [handleId frameworkCloseEmptyHandle]
  pure [currentSystem]

frameworkCoreAstBlueprint :: AstBlueprintSeed
frameworkCoreAstBlueprint =
  AstBlueprintSeed
    { astBlueprintSeedBoot =
        SeedLeaf
          (ImplementationTarget frameworkCloseEmptyImplementation)
    , astBlueprintSeedHanging = []
    }

frameworkAsBusiness ::
  Either [RecordError] FrameworkAsBusiness
frameworkAsBusiness = do
  currentSystems <-
    frameworkCoreEffectSystemDeclarations
  pure
    FrameworkAsBusiness
      { frameworkAsBusinessSchema = frameworkCoreSchemaV1
      , frameworkAsBusinessEffectSystems = currentSystems
      , frameworkAsBusinessAstSeed =
          frameworkCoreAstBlueprint
      , frameworkAsBusinessHandlerCoverage =
          [ handleId frameworkValidateFacadeHandle
          , handleId frameworkLowerCurdeHandle
          , handleId frameworkCompileCataHandle
          , handleId frameworkCloseEmptyHandle
          ]
      , frameworkAsBusinessClaims =
          [ "normal-curde-facade"
          , "single-input-read-chain"
          , "fix-cata-control-compilation"
          , "empty-business-terminal"
          ]
      }

encodeFrameworkAsBusiness :: FrameworkAsBusiness -> String
encodeFrameworkAsBusiness =
  show

decodeFrameworkAsBusiness ::
  String ->
  Either String FrameworkAsBusiness
decodeFrameworkAsBusiness currentText =
  case readMaybe currentText of
    Nothing ->
      Left "invalid FrameworkAsBusiness"
    Just currentFramework
      | frameworkAsBusinessSchema currentFramework
          /= frameworkCoreSchemaV1 ->
          Left "unsupported FrameworkAsBusiness schema"
      | otherwise ->
          Right currentFramework

interpretFrameworkAsBusiness ::
  [CoreId] ->
  FrameworkAsBusiness ->
  EmptyBusiness ->
  IO
    ( Either
        FrameworkInterpretError
        FrameworkSemanticObservation
    )
interpretFrameworkAsBusiness
  currentRuntimeDependencies
  currentFramework
  EmptyBusiness =
    case frameworkHandlerRegistry currentFramework EmptyBusiness of
      Left currentError ->
        pure (Left (FrameworkHandlerRegistrationFailed currentError))
      Right currentHandlers ->
        case
            prepareRuntime
              ( lowerCURDEDecl
                  (frameworkAsBusinessEffectSystems currentFramework)
                  (frameworkAsBusinessAstSeed currentFramework)
              )
          of
            Left currentError ->
              pure
                (Left (FrameworkRuntimePreparationFailed currentError))
            Right currentProgram -> do
              currentRun <-
                runRuntimeProgram
                  currentProgram
                  currentHandlers
                  frameworkRuntimeHooks
              let currentSnapshot =
                    runtimeRunSnapshot currentRun
                  currentControlSucceeded =
                    controlResultSucceeded
                      (runtimeRunControlResult currentRun)
                  currentExecutionStatus =
                    runtimeSnapshotExecutionStatus
                      (handleId frameworkCloseEmptyHandle)
                      currentSnapshot
                  currentClosed =
                    currentControlSucceeded
                      && currentExecutionStatus == ExecutionSucceeded
              pure
                ( if currentClosed
                    then
                      Right
                        FrameworkSemanticObservation
                          { frameworkObservationSemanticDigest =
                              semanticObservationDigest
                                currentFramework
                                currentExecutionStatus
                          , frameworkObservationControlSucceeded =
                              currentControlSucceeded
                          , frameworkObservationFinalExecutionStatus =
                              currentExecutionStatus
                          , frameworkObservationRuntimeDependencies =
                              currentRuntimeDependencies
                          }
                    else
                      Left
                        ( FrameworkRuntimeDidNotClose
                            (show currentRun)
                        )
                )

candidateHasNoPreviousCoreDependency ::
  CoreId ->
  FrameworkSemanticObservation ->
  Bool
candidateHasNoPreviousCoreDependency previousCore currentObservation =
  previousCore
    `notElem` frameworkObservationRuntimeDependencies currentObservation

frameworkHandlerRegistry ::
  FrameworkAsBusiness ->
  EmptyBusiness ->
  Either RegistryError HandlerRegistry
frameworkHandlerRegistry currentFramework currentTerminal = do
  withValidate <-
    registerR
      frameworkValidateFacadeHandle
      ( readHandler
          frameworkTokenCodec
          (const (pure (frameworkValidateToken currentFramework)))
      )
      emptyHandlerRegistry
  withLower <-
    registerR
      frameworkLowerCurdeHandle
      ( readHandlerUsingInput
          frameworkTokenCodec
          ( \currentInput ->
              pure
                ( frameworkLowerToken currentFramework
                    >>= useHandlerInput currentInput
                )
          )
      )
      withValidate
  withCompile <-
    registerR
      frameworkCompileCataHandle
      ( readHandlerUsingInput
          frameworkTokenCodec
          ( \currentInput ->
              pure
                ( frameworkCompileToken currentFramework
                    >>= useHandlerInput currentInput
                )
          )
      )
      withLower
  registerE
    frameworkCloseEmptyHandle
    ( discardingCommandHandler
        frameworkTokenCodec
        ( \_currentInput currentToken ->
            pure
              ( frameworkCloseToken
                  currentFramework
                  currentTerminal
                  currentToken
              )
        )
    )
    withCompile

frameworkValidateToken ::
  FrameworkAsBusiness ->
  Either RuntimeFailure String
frameworkValidateToken currentFramework =
  case
      validateEffectSystems
        (frameworkAsBusinessEffectSystems currentFramework)
    of
      [] ->
        Right
          ( sha256
              ( "facade:"
                  ++ show
                    (frameworkAsBusinessEffectSystems currentFramework)
              )
          )
      currentErrors ->
        Left
          (frameworkReadFailure ("facade validation failed: " ++ show currentErrors))

frameworkLowerToken ::
  FrameworkAsBusiness ->
  Either RuntimeFailure String
frameworkLowerToken currentFramework =
  case loweringValidationErrors currentLowering of
    [] ->
      Right
        ( sha256
            ( "lowering:"
                ++ show
                  (frameworkAsBusinessEffectSystems currentFramework)
                ++ ":"
                ++ show
                  (frameworkAsBusinessAstSeed currentFramework)
            )
        )
    currentErrors ->
      Left
        (frameworkReadFailure ("CURDE lowering failed: " ++ show currentErrors))
  where
    currentLowering =
      lowerCURDEDecl
        (frameworkAsBusinessEffectSystems currentFramework)
        (frameworkAsBusinessAstSeed currentFramework)

frameworkCompileToken ::
  FrameworkAsBusiness ->
  Either RuntimeFailure String
frameworkCompileToken currentFramework =
  case
      prepareRuntime
        ( lowerCURDEDecl
            (frameworkAsBusinessEffectSystems currentFramework)
            (frameworkAsBusinessAstSeed currentFramework)
        )
    of
      Left currentError ->
        Left
          (frameworkReadFailure ("cata control compile failed: " ++ show currentError))
      Right currentProgram ->
        Right
          ( sha256
              ( "cata:"
                  ++ show
                    (frameworkAsBusinessAstSeed currentFramework)
                  ++ ":hanging="
                  ++ show (runtimeProgramHangingCount currentProgram)
              )
          )

frameworkCloseToken ::
  FrameworkAsBusiness ->
  EmptyBusiness ->
  String ->
  Either RuntimeFailure ()
frameworkCloseToken currentFramework EmptyBusiness currentToken
  | null currentToken =
      Left (frameworkReadFailure "cata token is empty")
  | null (frameworkAsBusinessSchema currentFramework) =
      Left (frameworkReadFailure "framework schema is empty")
  | otherwise =
      Right ()

frameworkReadFailure :: String -> RuntimeFailure
frameworkReadFailure currentMessage =
  RuntimeFailure
    { runtimeFailurePhase = ReadPhase
    , runtimeFailureCommitState = NoExternalCommit
    , runtimeFailureMessage = currentMessage
    }

semanticObservationDigest ::
  FrameworkAsBusiness ->
  ExecutionStatus ->
  String
semanticObservationDigest currentFramework currentStatus =
  sha256
    ( intercalate
        "|"
        [ frameworkAsBusinessSchema currentFramework
        , show (frameworkAsBusinessEffectSystems currentFramework)
        , show (frameworkAsBusinessAstSeed currentFramework)
        , show (frameworkAsBusinessHandlerCoverage currentFramework)
        , show (frameworkAsBusinessClaims currentFramework)
        , show currentStatus
        ]
    )

frameworkRuntimeHooks :: RuntimeHooks
frameworkRuntimeHooks =
  RuntimeHooks
    { runtimeHookOperators = emptyPureOperatorRegistry
    , runtimeHookWait =
        \_ _ currentSnapshot ->
          pure (RuntimeGateReady currentSnapshot)
    , runtimeHookSuspense =
        \_ _ currentSnapshot ->
          pure (RuntimeGateReady currentSnapshot)
    , runtimeHookMiddleware =
        \_ _ currentAction -> currentAction
    , runtimeHookCallback =
        \_ _ currentAction -> currentAction
    , runtimeHookContext =
        \_ _ currentAction -> currentAction
    , runtimeHookLoop =
        \_ ->
          RuntimeLoopPolicy
            { runtimeLoopIterationLimit = 1
            , runtimeLoopStabilityPredicate = \_ _ -> True
            }
    }
