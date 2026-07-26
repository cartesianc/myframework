# Runtime execution boundary

`MyFramework.Runtime` is framework-author tooling over an already lowered
application. It is not a serializable authoring surface.

`prepareRuntime` rejects the complete lowering error set before compiling the
13-node control plan. `runRuntimeProgram` starts only the boot tree and creates
a fresh `HandleId` single-flight coordinator for every run. Hanging trees stay
inert metadata.

Runtime hooks implement wait, suspense, middleware, callback, context, loop,
and pure-operator behavior. The demand callback is internal and cannot be
replaced by a caller. No default hook silently ignores an explicit AST node.

The result contains the final immutable snapshot, the control result, and a
pure diagnosis overlay. Diagnosis does not mutate execution history or trigger
replay. Observation unavailability marks producer validity as `Suspect` while
preserving `ExecutionSucceeded`; an R failure remains a separate read channel.
Race cancellation with no final loser snapshot is reported as settlement
uncertainty instead of being accepted as safe success. The current Runtime
does not add retry, idempotency, transaction, compensation, `ana`, `hylo`, or
protocol-driven unfolding semantics.
