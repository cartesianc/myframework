
module Main (main) where

import Control.Monad
  ( unless )
import Data.Char
  ( ord )
import Data.List
  ( intercalate )
import Numeric
  ( showHex )
import System.Exit
  ( exitFailure )
import Text.Read
  ( readMaybe )

import MyFramework.Ast
  ( AstBlueprintSeed (..)
  , AstSeed (..)
  , AstTarget (..)
  )
import MyFramework.CURDE
import MyFramework.CURDE.Evidence
  ( CURDEClaimEvidence (..)
  , CURDEEvidenceBoundary (..)
  , CURDEEvidenceStatus (..)
  , CURDESemanticsReport (..)
  , curdeSemanticsReport
  )
import MyFramework.Self.ControlTrace
  ( compileControlTrace
  , controlTraceConstructorWitness
  )
import MyFramework.Self.Model
  ( canonicalSelfModelJson
  , selfEmitAuditHandle
  , selfModel
  , selfModelAstSeed
  , selfModelEffectSystems
  , selfModelJsonRoundTrip
  , selfModelTextRoundTrip
  , selfReadUserHandle
  , selfUpdateUserHandle
  )
import MyFramework.TrustBase.Evidence
  ( ClaimCatalog (..)
  , claimCatalogClaims
  )
import MyFramework.TrustBase.Types
  ( ClaimName (..)
  , renderSchemaId
  )

main :: IO ()
main =
  case selfModel of
    Left currentErrors -> do
      putStrLn
        ( renderFatalJson
            "record-lowering"
            (show currentErrors)
        )
      exitFailure
    Right currentSelfModel -> do
      let currentSystems =
            selfModelEffectSystems currentSelfModel
          currentBlueprint =
            selfModelAstSeed currentSelfModel
          baseReport =
            curdeSemanticsReport currentSystems currentBlueprint
          currentControlTrace =
            compileControlTrace currentSelfModel
          dischargedEvidence =
            map dischargeCompileTimeEvidence
              (curdeSemanticsReportEvidence baseReport)
          currentCatalog =
            curdeSemanticsReportCatalog baseReport
          expectedClaims =
            claimCatalogClaims currentCatalog
          observedClaims =
            map curdeClaimEvidenceName dischargedEvidence
          exactManifest =
            length (claimCatalogCoreClaims currentCatalog) == 20
              && length expectedClaims == 21
              && observedClaims == expectedClaims
          curdeRoundTrip =
            roundTrip ([C, U, R, D, E] :: [CURDE])
          handleKinds =
            map handleDeclKind
              (concatMap effectSystemDeclHandles currentSystems)
          handleRoundTrip =
            all roundTrip
              (concatMap effectSystemDeclHandles currentSystems)
          allKindsPresent =
            all (`elem` handleKinds) [C, U, R, D, E]
          selfRoundTrip =
            selfModelTextRoundTrip currentSelfModel
              && selfModelJsonRoundTrip currentSelfModel
          selfJsonPresent =
            not (null (canonicalSelfModelJson currentSelfModel))
          controlTraceReady =
            controlTraceConstructorWitness
              && either (const False) (const True) currentControlTrace
          missingImplementationRejected =
            rejectsMissingImplementation
              currentSystems
          staticFailures =
            filter isBlockingEvidence dischargedEvidence
          succeeded =
            exactManifest
              && curdeRoundTrip
              && handleRoundTrip
              && allKindsPresent
              && selfRoundTrip
              && selfJsonPresent
              && controlTraceReady
              && missingImplementationRejected
              && null staticFailures
      putStrLn
        ( renderReportJson
            baseReport
            dischargedEvidence
            exactManifest
            curdeRoundTrip
            handleRoundTrip
            allKindsPresent
            missingImplementationRejected
            succeeded
        )
      unless succeeded exitFailure

rejectsMissingImplementation :: [EffectSystemDecl] -> Bool
rejectsMissingImplementation currentSystems =
  any isExpectedFailure
    ( loweringValidationErrors
        ( lowerCURDEDecl
            currentSystems
            AstBlueprintSeed
              { astBlueprintSeedBoot =
                  SeedLeaf
                    (HandleTarget (handleRefFor selfEmitAuditHandle))
              , astBlueprintSeedHanging = []
              }
        )
    )
  where
    isExpectedFailure currentError =
      case currentError of
        MissingImplementation _ currentHandle _ ->
          currentHandle == handleId selfUpdateUserHandle
        _ ->
          False

dischargeCompileTimeEvidence ::
  CURDEClaimEvidence ->
  CURDEClaimEvidence
dischargeCompileTimeEvidence currentEvidence =
  case curdeClaimEvidenceStatus currentEvidence of
    EvidenceDeferredTo GenericRecordCompileTimeWitness
      | curdeClaimEvidenceName currentEvidence
          == ClaimName "curde-generic-record-identity" ->
          currentEvidence
            { curdeClaimEvidenceStatus = EvidenceEstablished
            , curdeClaimEvidenceObserved =
                "compiled Generic records whose selectors equal their Handle Symbols; typed handles are reused by records, inputs, R expressions, implementations, and AST references"
            }
    EvidenceDeferredTo PublicFacadeCompileTimeWitness
      | curdeClaimEvidenceName currentEvidence
          == ClaimName "curde-public-facade-boundary" ->
          currentEvidence
            { curdeClaimEvidenceStatus = EvidenceEstablished
            , curdeClaimEvidenceObserved =
                "this executable compiles against MyFramework.CURDE without importing CURDE.Core, DemandGraph, or any runtime module"
            }
    _ ->
      currentEvidence

isBlockingEvidence :: CURDEClaimEvidence -> Bool
isBlockingEvidence currentEvidence =
  case curdeClaimEvidenceStatus currentEvidence of
    EvidenceViolated -> True
    EvidenceBlocked _ -> True
    EvidenceEstablished -> False
    EvidenceDeferredTo _ -> False

roundTrip :: (Eq value, Read value, Show value) => value -> Bool
roundTrip currentValue =
  readMaybe (show currentValue) == Just currentValue

renderReportJson ::
  CURDESemanticsReport ->
  [CURDEClaimEvidence] ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  String
renderReportJson
  currentReport
  currentEvidence
  exactManifest
  curdeRoundTrip
  handleRoundTrip
  allKindsPresent
  missingImplementationRejected
  succeeded =
    jsonObject
      [ ("schema", jsonString (renderSchemaId (curdeSemanticsReportSchema currentReport)))
      , ("artifact", jsonString "curde-semantics-witness")
      , ("result", jsonString (if succeeded then "passed" else "failed"))
      , ( "frontend"
        , jsonObject
            [ ("effectSystemCount", show (2 :: Int))
            , ("handleKinds", jsonArray (map (jsonString . show) [C, U, R, D, E]))
            , ( "astDemandLeaf"
              , jsonString
                  (renderHandleId (handleId selfEmitAuditHandle))
              )
            , ( "crossChainRead"
              , jsonString
                  (renderHandleId (handleId selfReadUserHandle))
              )
            , ( "crossChainConsumer"
              , jsonString
                  (renderHandleId (handleId selfUpdateUserHandle))
              )
            ]
        )
      , ( "witnessChecks"
        , jsonObject
            [ ("curdeReadShowRoundTrip", jsonBool curdeRoundTrip)
            , ("handleDeclReadShowRoundTrip", jsonBool handleRoundTrip)
            , ("allKindsPresent", jsonBool allKindsPresent)
            , ("genericRecordCompileTime", jsonBool True)
            , ("typedHandleReuseCompileTime", jsonBool True)
            , ("publicFacadeCompileTime", jsonBool True)
            , ( "missingImplementationRejected"
              , jsonBool missingImplementationRejected
              )
            ]
        )
      , ( "claimManifest"
        , jsonObject
            [ ("catalog", jsonString (claimCatalogName (curdeSemanticsReportCatalog currentReport)))
            , ("coreCount", show (length (claimCatalogCoreClaims (curdeSemanticsReportCatalog currentReport))))
            , ("totalCount", show (length currentEvidence))
            , ("exact20Plus1", jsonBool exactManifest)
            ]
        )
      , ("claims", jsonArray (map renderEvidenceJson currentEvidence))
      , ( "observation"
        , jsonString (show (curdeSemanticsReportObservation currentReport))
        )
      ]

renderEvidenceJson :: CURDEClaimEvidence -> String
renderEvidenceJson currentEvidence =
  jsonObject
    [ ("name", jsonString (unClaimName (curdeClaimEvidenceName currentEvidence)))
    , ("status", jsonString (renderEvidenceStatus (curdeClaimEvidenceStatus currentEvidence)))
    , ("expected", jsonString (curdeClaimEvidenceExpected currentEvidence))
    , ("observed", jsonString (curdeClaimEvidenceObserved currentEvidence))
    ]

renderEvidenceStatus :: CURDEEvidenceStatus -> String
renderEvidenceStatus currentStatus =
  case currentStatus of
    EvidenceEstablished -> "established"
    EvidenceViolated -> "violated"
    EvidenceBlocked _ -> "blocked"
    EvidenceDeferredTo currentBoundary ->
      "deferred:" ++ show currentBoundary

renderFatalJson :: String -> String -> String
renderFatalJson currentStage currentObserved =
  jsonObject
    [ ("schema", jsonString "curde-semantics-witness.v1")
    , ("artifact", jsonString "curde-semantics-witness")
    , ("result", jsonString "failed")
    , ("stage", jsonString currentStage)
    , ("observed", jsonString currentObserved)
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
