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

The optional generated-source provenance boundary is documented in
`docs/SDK_SOURCE_ARTIFACT.md`. It consumes the existing erased CURDE and AST
configuration values; it is not another authoring facade.

The runtime compiler/executor boundary is documented in `docs/RUNTIME.md`.
It consumes a validated lowering result and is not another authoring facade.
