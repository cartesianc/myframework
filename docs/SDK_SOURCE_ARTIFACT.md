# Approved-core SDK source boundary

The SDK generator is a maintenance and release surface, not a fourth business
authoring surface. Business authors still provide only Effect System/CURDE,
AST, and Handler declarations.

## Authoritative input

`mkSdkSourceInput` accepts the existing serializable values:

```text
approved TrustBaseRef
SDK version
[EffectSystemDecl]
AstBlueprintSeed
Maybe erased Handler coverage
```

It derives an `SdkCoreLock` containing:

```text
approved core identity and artifact/manifest digests
canonical SDK surface digest
SDK lowering-semantics digest
```

The surface digest commits the canonical SDK version, erased CURDE systems,
AST seed, and optional Handler identities. The lock itself is excluded from
that calculation to avoid a circular digest. `buildSdkSourceReport` blocks an
invalid core reference, surface mismatch, lowering mismatch, duplicate system,
or unknown Handler identity before materialization.

Handler implementations, existential registry entries, closures, codecs,
runtime values, and environments never enter this serializable input.

## Package materialization

`MyFramework.SDK.Package.materializeApprovedSdkPackage` requires all three
promotion values:

```text
CoreManifest
PromotionRecord(Approved)
CurrentCorePointer
```

The pointer must select the record's candidate, the record must be approved,
and the candidate manifest must match. The approved artifact digest must equal
the SHA-256 payload digest of the source closure actually materialized in this
run.

A successful package contains:

- the complete standalone framework source closure;
- generated `MyFramework.Generated.SourceArtifact` provenance;
- `sdk-core-lock.read`;
- canonical SDK source report;
- embedded current-core pointer, core manifest, and approved promotion record;
- a package manifest containing every generated file digest.

`verifySdkPackage` independently rechecks the base artifact manifest, payload
digest, every generated file, the decoded core lock, and the embedded
promotion/current relationship. A pending record or a lock copied from another
surface is rejected.

## Determinism

Top-level effect systems and Handler coverage are canonicalized as sets. All
semantically ordered fields inside an `EffectSystemDecl` and the entire AST
seed preserve authoring order. The lowering digest is versioned explicitly.

The `sdk-package-witness` proves valid materialization and verification, plus
negative cases for pending promotion and tampered surface/lowering digests. It
runs in the ordinary release gate and is reproduced byte-for-byte by Stage1
and Stage2 in the heavy gate.

## CI/CD

`.github/workflows/ci.yml` runs the ordinary release pre-gate on pushes and
pull requests.

`.github/workflows/beta-sdk.yml` is an explicit maintenance action. It fails
closed unless these reviewed files exist:

```text
trustbase/core-manifest.json
trustbase/promotion.approved.json
trustbase/current.json
```

The workflow runs the ordinary and one-shot self-artifact gates, materializes
the SDK, verifies it, builds it independently, reruns the package witness from
inside the generated SDK, and uploads a beta archive. Publishing a GitHub
prerelease is a separate boolean workflow input and never approves a core.

## Semantic exclusions

The SDK boundary introduces no pipeline, retry policy, transaction semantics,
Handler execution semantics, `ana`, or `hylo`. It packages the already-frozen
`Fix + cata` framework semantics and their approved core identity.