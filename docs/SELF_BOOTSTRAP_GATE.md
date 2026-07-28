# Semantic self-bootstrap and artifact gate

The repository has two independent fixed-point obligations. Neither substitutes
for the other.

## Semantic closure

```text
Host TCB
  -> bound core0
  -> FrameworkAsBusiness expressed by normal CURDE/AST/Handler contracts
  -> EmptyBusiness
  -> normalized core0/core1 semantic observations
```

`core-self-interpret-witness` proves that the previous core runs the candidate,
the candidate uses the normal facade, `EmptyBusiness` closes recursion without
CURDE/Handler/host IO, core0 and core1 observations are exchangeable, and the
candidate has no runtime dependency on core0. A contaminated back-reference is
a required negative witness.

## Artifact fixed point

The heavy gate performs:

1. Stage0 materializes Stage1.
2. Stage1 independently builds and runs semantic, runtime, TrustBase,
   self-interpret, promotion, SDK package, self-model, and manifest witnesses.
3. Stage1 materializes Stage2.
4. Stage2 independently repeats the same witness set.
5. Stage1 and Stage2 source manifests reach a byte-stable fixed point.
6. Collected semantic/runtime evidence reaches a fixed point.
7. Corresponding Stage0/Stage1 and Stage1/Stage2 reports are byte-equal.
8. The promotion tool creates and verifies a candidate manifest and a
   `PromotionPending` record bound to the actual evidence digests.

Only `artifactSourceFiles` enter a materialized artifact. Each is classified as
reproduced source or an explicit trusted seed and receives a SHA-256 digest.

## Commands and status

Ordinary release validation:

```powershell
.\scripts\check-release.ps1
```

Passing it means `ready for self-artifact gate`.

One-shot heavy validation for an exact worktree fingerprint:

```powershell
.\scripts\check-release.ps1 -IncludeSelfArtifact
```

The marker prevents an accidental repeat on the same fingerprint. Timeout is
inconclusive. Passing means `self-artifact passed`, not `promotion approved`.

## Promotion separation

Validation never modifies `trustbase/current.json`. After the heavy gate, a
maintainer reviews the generated core manifest, pending record, TrustBase
classification, diff, and evidence. Only the explicit `core-promotion-tool
approve` command can produce an approved record and matching current pointer.

A beta SDK additionally requires those reviewed files and independently checks
that the approved core artifact digest is the payload it packages. CI cannot
turn a pending candidate into a current core.