# TrustBase boundary

MyFramework separates three boundaries that must not be conflated:

1. Build TCB: GHC, Stack/Cabal, OS, filesystem, and process execution.
2. Runtime Host TCB: the closed `HostKernel` needed to identify, verify, load,
   and call an already validated core.
3. Previous Framework Core: a round-local core that may be replaced after a
   candidate passes all gates.

## Explicit core binding

`TrustBaseRef` is serializable and records core identity, artifact digest,
manifest digest, schema, and kernel claims. It never contains a function or
runtime environment.

`BoundTrustBase` is existential and runtime-only. `bindTrustBase` accepts a
closed `HostKernel` plus an exact `TrustBaseRef`; it rejects core, artifact,
manifest, schema, or claim mismatch before invoking the loader. Business ASTs
do not contain a generic host escape.

The permanent HostKernel owns only verification/loading/bootstrap entry
capabilities. CURDE lowering, AST semantics, diagnosis, promotion policy, SDK
lowering, and business operations remain outside it.

## Rotating self-bootstrap core

`FrameworkAsBusiness` is the serializable core expression built from the normal
three authoring surfaces. `EmptyBusiness` is its nullary App-level recursive
base and creates no fake AST node. A candidate that loads or calls its previous
core after materialization is rejected.

Genesis core0 may interpret and materialize core1. Once core1 is explicitly
approved, the next round is core1 -> core2; core0 may remain only as archival or
rollback material, not an active dependency.

## Evidence and promotion

The TrustBase manifest catalogs all stable schemas and claim names, including
binding, semantic self-interpretation, promotion, and SDK package evidence.
Focused witnesses prove both positive and required negative cases.

`PromotionRecord` binds the actual semantic, artifact, and EmptyBusiness report
digests. It begins as `PromotionPending`. Approval is an explicit maintenance
action and creates both `promotion.approved.json` and `current.json`; ordinary
builds and release gates cannot do this.

The approved current core is implicit only at the business-user API. It remains
explicit in the SDK lock, package manifest, maintenance API, and repository
promotion records.

## Minimality claim

The project claims inclusion minimality only relative to the declared host
model and bootstrap obligations: removing any claimed HostKernel capability
must break a corresponding witness. It does not claim a globally minimal TCB
across all possible languages and implementations.