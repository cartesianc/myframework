# Architecture

## Semantic source

This repository is the only source of CURDE semantics. The bootstrap repository
is a reference implementation. The SDK repository is a lowering product.

## Authoring surfaces

The public model has exactly three authoring surfaces:

- Effect System
- AST
- Handler

Effect System and AST declarations are records or closed data that can be
serialized. Runtime functions, lambdas, handles, and environment objects never
enter serialized frontend values.

## CURDE

`C`, `U`, `D`, and `E` declare effectful handles. Their public result is a
status. A successful handler may also produce a private observation.

`R` declares a lazy typed value. Its source is its single input's observation
or R value, or a runtime Handler environment. A serializable `RExpr` supplies
Implementation arguments only from typed R references, literals, and closed
records/products. Business data transformations are explicit R Facts with
registered R handlers; there is no executable operator registry outside the
Effect System.

Every fact has at most one explicit input. A declared input is a real semantic
dependency and the handler must consume the input handle or state.

An Implementation binds a complete serializable argument expression to a
`C`, `U`, `D`, or `E` handle. There is no `impl R` in the first version.
`ImplementationDecl` values occur directly in AST `Leaf` or
`WithImplementation` nodes; lowering never accepts a separate implementation
registry. A `Leaf` containing an Implementation is a demand root.
`WithImplementation` only adds a lexical binding for roots in its child tree.
A Handle leaf must name a parameterless CUDE handle. Lowering derives the
implicit Unit Implementation for such a demanded handle.

The frontend does not assign retry, idempotency, transaction, compensation, or
CRUD operation semantics. Those properties belong to handlers or explicit AST
control semantics.

## Demand graph

Lowering builds:

```text
H      = registered CURDE handles
I      = AST-derived Implementations plus demanded implicit Unit bindings
Ebind  = handle-to-Implementation demand edges
Einput = the optional input edge of each handle
Euse   = R value references used by Implementations
B      = AST Leaf roots
G      = demandClosure(H union I, Ebind union Einput union Euse, B)
```

AST leaves are the only initial demand roots. No pipeline, `Boot.targets`,
global Implementation registry, or fourth frontend surface exists.

## Recursive AST

The internal tree is `Fix AstF`. The base functor contains:

```text
Leaf
WithImplementation
Chain
Parallel
Fallback
Race
Choice
Wait
Loop
Middleware
Callback
Suspense
Context
```

The current recursion kernel provides `Fix` and `cata`. Serializable frontend
records lower explicitly into the internal fixed point. Layout, path,
dependency, and validation projections fold that fixed point.

`Context` remains a versioned serializable AST constructor, but its semantics
are not frozen: lowering and the internal Control compiler reject it. Likewise,
`astBlueprintSeedHanging` must be empty. Hanging roots and Context never enter
the executable Control plan, demand graph, runtime layout, or diagnosis.

Coalgebra-based unfolds and refolds such as `ana` and `hylo` are deliberately
reserved for the later JSON-RPC boot framework.

Layout is read-only. Runtime listeners append events. A live cursor path must
refer to a node in the pre-run layout. Diagnosis overlays never mutate AST or
runtime history.

## Runtime

The runtime starts deterministically and does not automatically replay CURDE.
A validated boot `ControlDemand` is the only authority source: it mints an
internal execution permit containing `BootRunId`, `AstPath`, and the root
`DemandNodeId`. Every actual handler invocation requires that permit and emits
an authorization event containing the provenance, actual demand node, and
handle identity. Registration alone never grants execution authority.

The executable control behavior is:

- Chain executes in order.
- Parallel isolates branches and merges state conservatively.
- Fallback isolates failed branches and only continues after a failure proves
  that no external commit occurred.
- Race selects the first successful branch, requests cancellation of the
  others, drains them, and reports settlement uncertainty when cancellation
  cannot prove safety.
- Choice executes only the selected branch.
- Wait observes an explicit predicate or status.
- Loop seeks a semantic fixed point without replaying a completed CUDE handle.
- shared HandleId evaluation uses single-flight coordination.

Execution status, R status, observations, values, and validity are separate
state channels. An R failure never overwrites a successful CUDE history. An
unavailable private observation marks the producing handle `Suspect` without
changing its successful execution status.

## SDK source artifact

`MyFramework.SDK.SourceArtifact` canonically commits the existing erased Effect
System declarations, serializable AST seed, and optional Handler identity
coverage to an explicit `SdkCoreLock`. The surface and lowering digests are
validated before any files are written. Handler implementations remain
runtime-only, and no fourth business-authoring surface is introduced.

`MyFramework.SDK.Package` accepts only a `CoreManifest`, approved
`PromotionRecord`, and matching `CurrentCorePointer`. It materializes the
complete standalone source closure, embeds the approved records and lock, and
records every generated file digest. Verification rechecks both the base
artifact payload and the package files. A pending or mismatched core cannot be
used for SDK generation.

Neither module extends the recursion kernel. `Fix` and `cata` remain the only
implemented recursion-scheme contract; protocol-driven unfolding remains
reserved for the future JSON-RPC boot framework.

## TrustBase

The permanent host boundary and rotating framework core are separate.
`TrustBaseRef` is serializable identity; `BoundTrustBase` is the existential
runtime capability loaded only after the closed `HostKernel` verifies core,
artifact, manifest, schema, and claim identities. The normal business facade
never receives either value.

`FrameworkAsBusiness` uses the normal CURDE/AST/Handler contracts and accepts
`EmptyBusiness` as the nullary recursive base. The semantic witness compares
core0 and core1 normalized observations and rejects any candidate runtime
back-reference to core0. The artifact witness independently proves the
Stage1/Stage2 source fixed point.

Validation creates only a pending promotion record. Switching
`trustbase/current.json` requires an explicit approved record after the heavy
gate; normal builds and CI never perform that switch.
