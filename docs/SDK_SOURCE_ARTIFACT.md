# SDK source artifact boundary

`MyFramework.SDK.SourceArtifact` is an opt-in provenance renderer for generated
SDK trees. It is not a fourth authoring surface and it does not execute or
lower an application.

## Authoritative input

The boundary accepts the existing public values directly:

```text
[EffectSystemDecl]  from eraseEffectSystem
AstBlueprintSeed    from the AST configuration surface
Maybe coverage      erased HandleId metadata only
```

A source bundle is assembled from those values without another lowering step:

```haskell
sourceInput =
  SdkSourceInput
    { sdkSourceSdkVersion = frameworkVersion
    , sdkSourceEffectSystems = map eraseEffectSystem effectSystems
    , sdkSourceAstSeed = blueprintSeed
    , sdkSourceHandlerCoverage =
        Just (handlerCoverageFromIds (handlerRegistryIds registry))
    }
```

No second textual CURDE or AST syntax is parsed. `EffectSystemDecl` and
`AstBlueprintSeed` remain the authoritative configuration. The textual
`Read`/`Show` encoding appears only inside the output artifact so generated
code can retain an exact, zero-dependency provenance payload.

Handler implementations, existential registry entries, closures, codecs,
runtime values, and environment objects never enter the artifact. A caller may
derive optional coverage with:

```haskell
handlerCoverageFromIds (handlerRegistryIds registry)
```

Coverage answers only which stable handle identities were present. It does not
bind handlers and cannot affect execution.

## Determinism

`canonicalizeSdkSourceInput`:

- sorts top-level effect systems by stable identity and full erased
  declaration;
- preserves every sequence inside each `EffectSystemDecl`;
- preserves the complete `AstBlueprintSeed` without rewriting it;
- treats optional handler identities as a sorted set.

`buildSdkSourceReport` rebuilds the artifact and sorted issue list from that
canonical input. `sdkSourceReportCanonical` is the pure invariant check.

`renderGeneratedSdkSource` canonicalizes `SdkSourceInput` and produces a module
containing only two JSON string constants. Writing that module to disk is the
responsibility of external SDK generation tooling, which keeps this package and
its backend immutable.

## Semantic exclusions

This boundary has no pipeline, `needs`, policy, `Boot.targets`,
`NativeFactRule`, handler execution, retry, scheduling, or runtime adapter
semantics. It does not import runtime or Handler modules.

The current recursion contract remains `Fix` plus `cata`. The renderer retains
the serializable AST seed as data and does not add `ana`, `hylo`, `unfold`, or
any protocol-driven boot behavior. A future JSON-RPC boot framework may define
that separate protocol boundary.

## TrustBase

The artifact and report carry explicit `.v1` schema identifiers, but this
opt-in renderer is not automatically promoted into a TrustBase manifest.
A release process may add those schemas and claims when the generated-source
artifact becomes a required promotion input.
