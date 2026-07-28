# AST 唯一执行入口与运行来源证明

状态：已实现；本地语义门禁通过，尚未执行 core promotion gate
适用范围：`myframework` 的 EffectSystem、AST、lowering、runtime 与 handler 边界
本文件只冻结执行来源，不冻结尚未确认的 AST 控制节点语义。

## 1. 问题

框架前台目前只应存在两个可序列化的语义来源：

```text
EffectSystemDecl
AstBlueprintSeed
```

其中：

- `EffectSystemDecl` 声明 C/U/R/D/E handle、单一 input、schema、private/export；
- `AstBlueprintSeed` 声明 boot 控制结构和 `ImplementationDecl` 参数绑定；
- handler 是 runtime-only 的操作语义实现；
- 注册 handler 只表示“实现可供绑定”，不表示“允许执行”。

改造前，`runRuntimeProgram` 的正常路径虽已从 AST boot tree 启动，但整个模块
边界尚未强制这一原则：

1. 根 Facade 暴露了 `invokeCude` 与 `invokeRead`，允许绕过 AST 直接调用 handler；
2. `OperatorRef`/`PureOperatorRegistry` 在 EffectSystem 之外形成了第二条可执行
   数据变换路径；
3. `Context` 已有 lowering/runtime 行为，但其语义尚未冻结；
4. `hanging` 当前不会被普通 boot 自动执行，但已经参与 lowering、implementation
   catalog、demand graph、layout 和 diagnosis，因此并非无语义字段。

在这些问题解决之前，框架不能声称“只有进入 AST 的语义才能运行”。

## 2. 冻结公理

### E1：AST 是唯一运行入口

任何 runtime 求值必须能够追溯到当前 `BootRun` 中由 AST cata 解释产生的唯一
workflow node。

不存在对应 AST provenance 的 handler、R 求值、CUDE 操作或 runtime extension
不得执行。

### E2：注册不产生执行权

```text
registered(handle) ≠ executable(handle)
```

handler registry 只保存 runtime implementation。一个 handle 即使已经注册，只要
不在 AST boot demand closure 中，就必须始终保持 `Unused`。

### E3：只有 workflow demand 可以启动 Effect 链

合法启动链固定为：

```text
AstBlueprintSeed.boot
  -> Fix AstF
  -> cata
  -> WorkflowNode / ControlDemand
  -> DemandNode
  -> EffectSystem input closure
  -> R / Implementation parameter binding
  -> CUDE handler invocation
```

runtime 不得提供从 `HandleId`、handler registry 或任意用户函数直接启动 Effect
链的公共入口。

### E4：EffectSystem 是所有 Fact 求值的语义边界

运行过程中出现的每个 R/C/U/D/E 都必须：

1. 在某个 `EffectSystemDecl` 中声明；
2. 通过 imports/private/exports 检查；
3. 位于某个 AST demand 的依赖闭包；
4. 由 runtime demand evaluator 按 input 关系到达；
5. 在调用 handler 前携带不可伪造的 AST execution provenance。

### E5：Implementation 只绑定参数

`ImplementationDecl` 只负责把已经声明的 R 数据绑定到 CUDE 参数，不产生新的
操作语义。

允许的参数表达限于已冻结、可序列化、无运行时函数逃逸的结构：

```text
R reference
literal
record/product construction
经过单独确认的纯结构 field selection
```

任意业务数据变换必须声明为 EffectSystem 中的 R Fact，并由该 R 的 handler 实现。

### E6：未冻结语义不可运行

一个构造器、引用或 runtime hook 没有经过语义冻结时，必须满足以下至少一项：

1. 不从公共 Facade 导出；
2. lowering 无条件拒绝；
3. 只作为不可执行的版本化数据保留。

“已经有代码但暂时没有文档”不构成合法状态。

## 3. 唯一合法执行模型

### 3.1 编译期

输入：

```text
EffectSystemDecl
AstBlueprintSeed
```

lowering 必须完成：

```text
resolve every AST target
resolve every Implementation target
resolve every R reference
validate EffectSystem visibility
validate input closure
validate boot reachability
reject every unsupported/unfrozen constructor
```

只有无错误结果才可生成 opaque `RuntimeProgram`。

`RuntimeProgram` 的构造器不得公开；调用者不能自行构造 `ControlTree`、
`DemandGraph` 或 executable root。

### 3.2 cata 到 workflow

AST 的 `Fix AstF` 只允许通过框架内部固定的 cata algebra 编译成 workflow。

当前阶段：

- 不允许前台携带 Haskell algebra；
- 不存在 `AlgebraRef` 或按字符串查找 algebra 的 registry；
- 不实现 paramorphism、prepromorphism、zygomorphism 或其他附加 algebra；
- 若未来需要 AST context/algebra，必须另立设计并先解决可序列化和执行授权问题。

### 3.3 boot

一次运行只接受一个经过验证的 boot workflow root。

解释器访问 `ControlDemand` 时创建内部 execution permit。permit 至少绑定：

```text
BootRunId
AstPath
DemandNodeId
```

permit 的构造器和创建函数均不得暴露给 Facade、业务代码或 handler。

### 3.4 Effect 链求值

runtime 使用 permit 从 root demand 反向寻找 prerequisite：

```text
root demand
  -> target input
  -> R dependency
  -> observation source
  -> upstream effect
```

链上每次 handler invocation 继续携带同一个 boot provenance，并记录当前
`DemandNodeId` 和 handle identity。

input 链之外的注册 handle 不得被扫描、预热或执行。

### 3.5 handler

handler 公共 API 只允许：

```text
construct
register
validate contract
```

实际 invocation 只能由 runtime internal API 完成，并要求 execution permit。

以下函数不得继续作为业务 Facade：

```text
invokeCude
invokeRead
```

测试 handler 本身时，可以测试纯 handler 函数；不得把“直接调用 registry 中的
handler”作为框架运行路径。

## 4. 未冻结语义

### 4.1 OperatorRef

`OperatorRef`、`applyOperator` 与 `PureOperatorRegistry` 未经语义确认，并在
EffectSystem 之外引入了可执行函数入口。

当前决定：

```text
OperatorRef is not part of the language.
```

它不得出现在公开 CURDE Facade、可执行 R expression 或 RuntimeHooks 中。

需要数据变换时，前台声明一个新的 R Fact：

```text
sourceR : R Source
derivedR : R Target
  input = sourceR
```

变换操作由 `derivedR` handler 保证。

### 4.2 Context

`Context` 的传递方向、生命周期、与 recursion scheme algebra 的关系尚未冻结。

当前决定：

```text
Context nodes are rejected by lowering.
```

在独立设计获批前，不允许 runtime 执行 `controlContextCallback`。

### 4.3 Hanging

`hanging` 是否属于监听入口、条件启动入口、外部事件入口或附属 workflow 尚未冻结。

当前决定：

```text
astBlueprintSeedHanging must be empty.
```

非空 hanging 必须在 lowering 阶段失败，并且不得进入：

```text
implementation catalog
demand graph
control plan
runtime layout
diagnosis
```

保留字段只为未来 schema 演进，不表示已经获得运行语义。

## 5. 改造结果

| 改造前问题 | 已实现结果 |
|---|---|
| `runRuntimeProgram` 只运行 boot，但 ControlPlan 仍保存 hanging | ControlPlan 只含唯一 boot tree |
| cata 产生 `ControlDemand` 后进入 demand evaluator | 保留，并且这里是唯一 permit mint 点 |
| `invokeCude`/`invokeRead` 从根 Facade 可见 | 已移入隐藏的 `Handler.Internal`，并要求 permit |
| `OperatorRef` 可在 implementation 参数中执行 | 已从 CURDE Facade、RExpr 和 RuntimeHooks 删除 |
| `Context` 可 lowering 并调用 runtime callback | lowering/Control 编译器拒绝，runtime hook 已删除 |
| hanging 会参与 lowering、graph、control、diagnosis | 非空时立即报错，不产生 executable projection |
| handler 注册后可被直接调用 | 公共 Handler 只保留构造、注册和 contract API |
| invocation 没有可审计来源 | RuntimeEvent 记录 boot/path/root node/actual node/handle |

## 6. 实现顺序

1. 收紧公共 Handler Facade，去掉无 AST provenance 的 invocation API。
2. 增加 runtime-internal execution permit，由 `ControlDemand` 解释点创建。
3. 将 permit 贯穿 demand session 和 handler invocation。
4. 删除 `OperatorRef` 公共表达及 runtime operator registry。
5. 将现有 operator witness 改写为显式 R Fact 链。
6. lowering 拒绝 Context。
7. lowering 拒绝非空 hanging，并确保它不影响任何图或 catalog。
8. 为每次实际 invocation 记录 `BootRunId/AstPath/DemandNodeId/HandleId`。
9. 完成正例、负例和 claim catalog 后，才允许更新 self-bootstrap evidence。

## 7. 必须通过的 witness

### 正例

```text
ast-boot-starts-effect-chain
boot-demand-closes-exact-input-chain
implementation-is-reached-only-from-ast
handler-invocation-has-ast-provenance
registered-and-reachable-handler-runs
```

### 负例

```text
registered-but-unreachable-handler-never-runs
direct-handler-invocation-is-not-public
unknown-ast-target-is-rejected
operator-ref-is-rejected
unfrozen-context-is-rejected
non-empty-hanging-is-rejected
missing-execution-permit-cannot-invoke-handler
effect-outside-boot-closure-remains-unused
```

### 固定点影响

以下 self-bootstrap/promotion claim 在上述 witness 通过前均不得重新签发：

```text
candidate-runs-as-framework-business
semantic-fixed-point-passed
core0-core1-exchangeable
promotion-approved
```

artifact/source closure fixed point 可以单独验证，但不能替代执行来源证明。

## 8. 非目标

本文件不：

- 冻结 hanging 的未来行为；
- 冻结 Context 的未来行为；
- 增加新的 recursion scheme；
- 允许前台携带 executable algebra；
- 将 handler 函数序列化；
- 将二进制 core 引入 Facade；
- 改变 C/U/R/D/E 和单一 input 的既有设计。

## 9. 完成定义

只有同时满足以下条件，才能声称“AST 是唯一执行入口”：

```text
所有公开运行入口都要求 validated RuntimeProgram
所有 RuntimeProgram 都来自 EffectSystemDecl + AstBlueprintSeed lowering
所有 handler invocation 都持有 AST execution provenance
所有执行中的 handle 都属于 boot demand closure
所有未冻结语义在 lowering 阶段失败
不存在 EffectSystem 之外的可执行 operator registry
负例 witness 全部通过
```

## 10. Verification status

Passed local gates:

- stack build
- stack exec curde-semantics-witness
- stack exec curde-runtime-witness
- scripts/check-ast-execution-boundary.ps1

Established runtime claim: curde-runtime-ast-execution-provenance.

Compile-negative checks:

- direct-handler-invocation-is-not-public
- operator-ref-is-rejected
- missing-execution-permit-cannot-invoke-handler

No self-artifact, release, or core-promotion gate was run.
The source closure and promotion records require the separate promotion workflow.