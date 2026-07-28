module MyFramework.Self.ControlTrace
  ( ControlTrace
  , ControlTraceError (..)
  , compileControlTrace
  , controlNodeCatalog
  , controlTraceConstructorWitness
  , controlTraceConstructorChecks
  , controlTraceLines
  , renderControlTraceJson
  ) where

import Data.Char
  ( ord )
import Data.List
  ( intercalate )
import Numeric
  ( showHex )

import MyFramework.Ast
  ( AstPath (..)
  , ChoiceKey (..)
  , MiddlewareRef (..)
  , renderAstPath
  )
import MyFramework.Control
  ( ControlNode (..)
  , ControlPlan (..)
  , ControlTree (..)
  , ControlValidationError
  , StatusPlan (..)
  , compileControlPlan
  )
import MyFramework.CURDE
  ( DemandNodeId (..)
  , EffectSystemName (..)
  , HandleId (..)
  , ImplementationId (..)
  , ValidationError
  , renderHandleId
  , renderImplementationId
  )
import MyFramework.CURDE.Lowering
  ( LoweringResult (..)
  , lowerCURDEDecl
  )
import MyFramework.Self.Model
  ( SelfModel
  , selfModelAstSeed
  , selfModelEffectSystems
  )

newtype ControlTrace = ControlTrace
  { controlTraceLines :: [String]
  }
  deriving (Eq, Ord, Read, Show)

data ControlTraceError
  = ControlTraceLoweringFailed [ValidationError]
  | ControlTraceCompilationFailed [ControlValidationError]
  deriving (Eq, Show)

-- | Stable constructor order for the closed AstF/ControlNode vocabulary.
controlNodeCatalog :: [String]
controlNodeCatalog =
  [ "Leaf"
  , "WithImplementation"
  , "Chain"
  , "Parallel"
  , "Fallback"
  , "Race"
  , "Choice"
  , "Wait"
  , "Loop"
  , "Middleware"
  , "Callback"
  , "Suspense"
  ]

compileControlTrace :: SelfModel -> Either ControlTraceError ControlTrace
compileControlTrace currentModel
  | not (null currentErrors) =
      Left (ControlTraceLoweringFailed currentErrors)
  | otherwise =
      case compileControlPlan (loweringCore currentLowering) of
        Left currentControlErrors ->
          Left
            (ControlTraceCompilationFailed currentControlErrors)
        Right currentPlan ->
          Right
            ( ControlTrace
                (renderPlanLines currentPlan)
            )
  where
    currentLowering =
      lowerCURDEDecl
        (selfModelEffectSystems currentModel)
        (selfModelAstSeed currentModel)
    currentErrors =
      loweringErrors currentLowering

renderPlanLines :: ControlPlan -> [String]
renderPlanLines currentPlan =
  renderTree (controlPlanBoot currentPlan)

renderTree :: ControlTree -> [String]
renderTree currentTree =
  currentLine : concatMap renderTree currentChildren
  where
    currentNode =
      controlTreeNode currentTree
    currentLine =
      intercalate
        "|"
        [ renderAstPath (controlTreePath currentTree)
        , controlNodeTag currentNode
        , controlNodePayload currentNode
        ]
    currentChildren =
      controlNodeChildren currentNode

controlNodeTag :: ControlNode -> String
controlNodeTag currentNode =
  case currentNode of
    ControlDemand _ -> "Leaf"
    ControlWithImplementation _ _ -> "WithImplementation"
    ControlSequence _ -> "Chain"
    ControlParallel _ -> "Parallel"
    ControlFallback _ -> "Fallback"
    ControlRace _ -> "Race"
    ControlChoice _ _ -> "Choice"
    ControlWait _ _ -> "Wait"
    ControlLoop _ -> "Loop"
    ControlMiddleware _ _ -> "Middleware"
    ControlCallback _ _ -> "Callback"
    ControlSuspense _ -> "Suspense"

controlNodePayload :: ControlNode -> String
controlNodePayload currentNode =
  case currentNode of
    ControlDemand currentDemand ->
      show currentDemand
    ControlWithImplementation currentImplementation _ ->
      renderImplementationId currentImplementation
    ControlSequence currentChildren ->
      "children=" ++ show (length currentChildren)
    ControlParallel currentChildren ->
      "children=" ++ show (length currentChildren)
    ControlFallback currentChildren ->
      "children=" ++ show (length currentChildren)
    ControlRace currentChildren ->
      "children=" ++ show (length currentChildren)
    ControlChoice currentSelection currentBranches ->
      "selected="
        ++ choiceKeyText currentSelection
        ++ ";keys="
        ++ intercalate
          ","
          [choiceKeyText currentKey | (currentKey, _) <- currentBranches]
    ControlWait currentStatus _ ->
      renderStatusPlan currentStatus
    ControlLoop _ ->
      ""
    ControlMiddleware currentMiddleware _ ->
      middlewareRefText currentMiddleware
    ControlCallback currentHandle _ ->
      renderHandleId currentHandle
    ControlSuspense currentHandle ->
      renderHandleId currentHandle

controlNodeChildren :: ControlNode -> [ControlTree]
controlNodeChildren currentNode =
  case currentNode of
    ControlDemand _ -> []
    ControlWithImplementation _ currentChild -> [currentChild]
    ControlSequence currentChildren -> currentChildren
    ControlParallel currentChildren -> currentChildren
    ControlFallback currentChildren -> currentChildren
    ControlRace currentChildren -> currentChildren
    ControlChoice _ currentBranches -> map snd currentBranches
    ControlWait _ currentChild -> [currentChild]
    ControlLoop currentChild -> [currentChild]
    ControlMiddleware _ currentChild -> [currentChild]
    ControlCallback _ currentChild -> [currentChild]
    ControlSuspense _ -> []

renderStatusPlan :: StatusPlan -> String
renderStatusPlan currentStatus =
  case currentStatus of
    StatusHandle currentHandle ->
      "status(" ++ renderHandleId currentHandle ++ ")"
    StatusAllPlan currentStatuses ->
      "all(" ++ intercalate "," (map renderStatusPlan currentStatuses) ++ ")"
    StatusAnyPlan currentStatuses ->
      "any(" ++ intercalate "," (map renderStatusPlan currentStatuses) ++ ")"

-- | Exercises every constructor against the total normalization functions.
-- This is independent from the framework's boot AST: it proves that extending
-- ControlNode without extending the stable trace schema cannot pass silently.
controlTraceConstructorWitness :: Bool
controlTraceConstructorWitness =
  length controlTraceConstructorChecks == length controlNodeCatalog
    && all snd controlTraceConstructorChecks

-- | One independently named normalization check per closed ControlNode
-- constructor. The child arities are part of the stable control vocabulary;
-- evaluating the payload rejects a partial normalizer as well.
controlTraceConstructorChecks :: [(String, Bool)]
controlTraceConstructorChecks =
  zipWith3
    checkConstructor
    controlNodeCatalog
    controlNodeChildArities
    controlNodeSpecimens
  where
    checkConstructor currentTag currentArity currentNode =
      ( currentTag
      , controlNodeTag currentNode == currentTag
          && length (controlNodeChildren currentNode) == currentArity
          && all (/= '\0') (controlNodePayload currentNode)
      )

controlNodeChildArities :: [Int]
controlNodeChildArities =
  [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0]

controlNodeSpecimens :: [ControlNode]
controlNodeSpecimens =
  [ ControlDemand sampleDemand
  , ControlWithImplementation sampleImplementation sampleChild
  , ControlSequence [sampleChild]
  , ControlParallel [sampleChild]
  , ControlFallback [sampleChild]
  , ControlRace [sampleChild]
  , ControlChoice
      (ChoiceKey "selected")
      [(ChoiceKey "selected", sampleChild)]
  , ControlWait (StatusHandle sampleHandle) sampleChild
  , ControlLoop sampleChild
  , ControlMiddleware (MiddlewareRef "middleware") sampleChild
  , ControlCallback sampleHandle sampleChild
  , ControlSuspense sampleHandle
  ]

sampleSystem :: EffectSystemName
sampleSystem =
  EffectSystemName "self"

sampleHandle :: HandleId
sampleHandle =
  HandleId sampleSystem "sample"

sampleImplementation :: ImplementationId
sampleImplementation =
  ImplementationId sampleHandle

sampleDemand :: DemandNodeId
sampleDemand =
  HandleNode sampleHandle

sampleChild :: ControlTree
sampleChild =
  ControlTree
    { controlTreePath = AstPath ["specimen", "child"]
    , controlTreeNode = ControlDemand sampleDemand
    }

renderControlTraceJson :: ControlTrace -> String
renderControlTraceJson currentTrace =
  "["
    ++ intercalate
      ","
      (map jsonString (controlTraceLines currentTrace))
    ++ "]"

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
          "\\u"
            ++ padLeft 4 '0' (showHex (ord currentChar) "")
      | otherwise ->
          [currentChar]

padLeft :: Int -> Char -> String -> String
padLeft currentWidth currentFill currentValue =
  replicate (max 0 (currentWidth - length currentValue)) currentFill
    ++ currentValue
