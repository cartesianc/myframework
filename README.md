# myframework

`myframework` is the source repository for the CURDE framework compiler and
runtime.

The repository has exactly three visible authoring surfaces:

1. Effect System: serializable CURDE handle and value declarations.
2. AST: serializable boot roots and control structure.
3. Handler: typed runtime implementations outside the serializable frontend.

The internal AST is recursive and its static projections use a catamorphism.
The runtime interprets the same control nodes without lowering them to the
legacy pipeline executor.

`ana`, `hylo`, and other unfold/refold schemes are intentionally deferred until
the JSON-RPC boot framework needs a protocol-driven unfolding boundary.

## Repository boundaries

- `myframework` is the semantic source.
- `dsl-bootstrap` is a migration reference and behavioral oracle.
- `dsl-sdk` is generated output. It is never edited as a semantic source.
- TrustBase contracts migrate as pure schema, evidence, manifest, and
  fixed-point validation code. Release promotion is a separate operation.

## Current self-bootstrap status

The implementation now contains both required fixed points:

- `core0 -> FrameworkAsBusiness -> EmptyBusiness` semantic self-interpretation;
- Stage1/Stage2 source and evidence reproducibility.

`TrustBaseRef`, existential `BoundTrustBase`, the closed `HostKernel`, explicit
promotion records, current-core pointers, approved-core SDK locks, and a
standalone `sdk-lower` package materializer are part of the same source
closure. Focused semantic, binding, promotion, and SDK package witnesses must
all pass before release validation starts.

Promotion is still deliberately separate from validation. A candidate is not
the current core until the heavy self-artifact gate passes and a maintainer
explicitly approves its pending promotion record. Only that approved core may
feed the beta SDK workflow.

The repository current pointer now selects the explicitly approved `core1`;
the retained `core0` manifest is archival/rollback evidence and is not an
active runtime dependency.

## Build policy

Development batches related semantic changes before compiling. Focused
semantic evidence is added with the implementation, but expensive release and
self-artifact promotion gates are not part of ordinary iteration.

```powershell
stack --work-dir .stack-work-codex build
stack --work-dir .stack-work-codex exec curde-semantics-witness
stack --work-dir .stack-work-codex exec curde-runtime-witness
```

See `docs/ARCHITECTURE.md` for the frozen semantic boundary.

The approved-core SDK lowering boundary is documented in
`docs/SDK_SOURCE_ARTIFACT.md`. It consumes the existing erased CURDE and AST
configuration values, binds their canonical surface to `SdkCoreLock`, and
materializes a standalone source package; it is not another authoring facade.

The runtime compiler/executor boundary is documented in `docs/RUNTIME.md`.
It consumes a validated lowering result and is not another authoring facade.
