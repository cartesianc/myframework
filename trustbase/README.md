# Promoted TrustBase core

This directory is maintenance state, not a business authoring surface and not
part of the materialized framework source closure.

- `current.json` selects the active rotating framework core.
- `core-manifest.json` is the selected core's content-addressed manifest.
- `promotion.approved.json` is the explicit maintainer decision bound to the
  semantic, artifact, and EmptyBusiness evidence digests.
- `promotion.pending.json` preserves the pre-decision record.
- `core0-manifest.json` is the previous genesis core retained for audit and
  rollback; the active core must not depend on it.
- `evidence/core1/` preserves the gate reports used for this promotion.

The current core is `core1`. Future rounds must use core1 as the previous core,
produce a separately identified candidate (for example core2), pass the same
semantic and artifact gates, and receive a new explicit approval. Editing only
`current.json` is not a valid promotion.