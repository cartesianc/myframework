# MyFramework 自举闭包与 TrustBase 轮换设计

## 0. 文档状态

本文定义 `myframework` 从“自描述 fixture + 可复现 artifact”推进到“语义闭合、可轮换
TrustBase 的自举 core”所需的设计。

当前工作树已经实现并通过聚焦 witness：

- framework 通过正常 CURDE/AST/Handler 前台表达为 `FrameworkAsBusiness`；
- previous compiled core 通过显式 `BoundTrustBase` 解释 candidate；
- candidate 接收 nullary `EmptyBusiness` 并闭合递归；
- core0/core1 的规范化语义观察可交换，且 candidate 对 core0 零运行时回指；
- `TrustBaseRef`、闭合 `HostKernel`、PromotionRecord 与 current pointer 已实现；
- Stage0/Stage1/Stage2 artifact fixed-point 协议保留；
- approved-core `SdkCoreLock`、standalone `sdk-lower`、package verifier 与 beta workflows 已实现。

当前工作树已经通过普通 release pre-gate 与重型 self-artifact gate，maintainer
已明确批准 `core1`，仓库的 approved record 与 current pointer 已生成。`core0` 仅作为
归档/回滚材料保留，不在 core1 的活动依赖闭包内。`0.1.0-beta.1` 已从 approved
core1 实际物化，并通过 package verify、独立构建、SDK witness、semantic witness 与 TrustBase report。

## 1. “可用”的分层定义

不能把所有可用性压缩成一个布尔值。

| 使用目标 | 当前状态 | 结论 |
|---|---|---|
| 研究 CURDE、单 input、R/Implementation、AST 控制语义 | 已有实现与 witness | 可用作实验核心 |
| 运行受当前 Handler/runtime 支持的业务模型 | 已有实现，但业务验收面仍需逐步扩展 | 有条件可用 |
| 验证源码 artifact 可独立构建并复现 | 协议与历史 gate 已有；当前改动待重跑 heavy gate | 待本轮证据 |
| 声称 framework 已经以自身语义表达自身 | `FrameworkAsBusiness` 与 semantic witness 已通过聚焦验证 | 已实现，待 release gate 固化 |
| 将 candidate 晋升为下一轮 TrustBase core | core1 已通过 heavy gate 并由 maintainer 明确批准 | promotion approved |
| 由 CI/CD 生成并发布可信 SDK | approved core1、sdk-lower、独立验证与 workflows 已实现 | 0.1.0-beta.1 本地端到端复验通过 |

对普通 CURDE/runtime 实验而言，这些缺口不会让全部代码失去用途。对项目的最终目标
——可自举 framework core 与可信 SDK 源——而言，它们是核心可用性条件，而不是可选优化。

## 2. 两条固定点不能混为一谈

### 2.1 语义自举闭包

```text
Host TCB
   │
   ▼
core_n 作为 Previous Framework TrustBase
   │ 解释
   ▼
core_(n+1) 作为 FrameworkAsBusiness
   │ 接收
   ▼
EmptyBusiness
```

这条链回答：

- 谁解释 candidate core；
- candidate 是否真由当前前台表达；
- framework 作为 App 后遗留的业务参数如何终止；
- candidate 是否能承担下一轮 previous core 的职责。

### 2.2 产物生成闭包

```text
artifact_0
   │ materialize
   ▼
artifact_1
   │ materialize
   ▼
artifact_2
```

这条链回答：

- 物化是否脱离父目录；
- 文件集合、来源与 digest 是否稳定；
- 同一版本能否重新生成同一规范化产物。

产物 fixed point 不能代替语义 fixed point。Stage2 也不能代替 `EmptyBusiness`：
Stage2 是第二次生成的 framework package；`EmptyBusiness` 是 framework-as-App 所需业务
参数的递归基例。

## 3. 两类外部边界

自举系统需要区分两类经常都被称为 TrustBase 的边界。

### 3.1 Host TCB：不参与 framework 轮换

Host TCB 还要再区分两部分：

- **Build TCB**：GHC、Stack/Cabal，以及物化 artifact 所需的 OS、filesystem 和 process；
- **Runtime Host TCB**：OS/loader、内容寻址实现，以及加载和调用已验证 core 的最小入口。

Host TCB 不能由本轮 framework 证明完全消除。本文把它视为自举链外部的不动边界。
这更接近 trusted computing base 和反射塔终止问题；哥德尔不完备可以作为类比，但不是
本工程结论的直接证明。

编译器、OS 和 loader 仍在 Haskell 值之外；不能声称一个 record 已经包含或消除了完整
Host TCB。需要被最小化和证明的是 myframework **直接可调用的 runtime surface**：
它必须收缩为封闭、带类型的 `HostKernel`，而不是一个通用逃逸层。概念接口为：

```haskell
data HostKernel = HostKernel
  { readArtifactBytes  :: ArtifactRef -> IO ByteString
  , digestArtifact     :: ByteString -> Digest
  , verifyCoreManifest :: TrustBaseRef -> ByteString -> Either KernelError VerifiedCore
  , loadVerifiedCore   :: VerifiedCore -> IO BoundTrustBase
  }

runSelfBootstrap ::
  BoundTrustBase ->
  BootstrapRound ->
  IO BootstrapEvidence
```

唯一允许启动一次自举求值的语义外入口是顶层、带类型的 `runSelfBootstrap`；
其内部只能调用 `HostKernel` 的闭合能力。不得向 AST 添加
`UnsafeIO`、`RawHostCall` 或任意函数注入之类的通用逃逸节点。

`HostKernel` 可以负责：

- 按精确的 artifact reference 读取字节；
- 计算并比较 digest；
- 验证 manifest、schema 和 core identity；
- 加载已经验证的 core；
- 调用固定 bootstrap entry point；
- 收集状态与不可伪造 evidence。

`HostKernel` 不得负责：

- CURDE lowering；
- AST 或 handler 语义；
- diagnosis 与重放策略；
- promotion policy；
- SDK lowering；
- 业务操作；
- 动态插件或任意 IO。

#### 3.1.1 最小性的可证明边界

给定固定宿主模型 `M` 和义务集合 `O`，可以要求能力集合 `K` 满足：

```text
Sufficient(M, O, K)

InclusionMinimal(M, O, K) =
  Sufficient(M, O, K)
  and
  for every p in K:
    not Sufficient(M, O, K without p)
```

这证明的是相对于 `M` 与 `O` 的包含最小性：`K` 足够，并且逐项移除任一能力都会产生
一个负 witness。可以使用有限模型检查、SMT、capability reachability、remove-one
测试，或后续的 proof assistant 证明。

本文不宣称 `HostKernel` 是所有可能实现和架构中的全局绝对最小 TCB。对图灵完备程序，
这种声明通常会退化到程序等价、停机或描述复杂度问题，既非当前 gate 所需，也不应作为
promotion claim。

### 3.2 Previous Framework Core：每轮可替换

`core_n` 对 `core_(n+1)` 而言位于语义外部，但它不是永久内核。只要 candidate 通过
语义闭包、exchangeability、artifact 和 promotion gate，下一轮就变为：

```text
core_(n+1) as Previous Framework Core
  -> core_(n+2) as candidate
  -> EmptyBusiness
```

因此需要保留：

- 永久但尽可能小的 Host TCB；
- round-local、可轮换的 Previous Framework Core。

不得把整个 runtime、materializer 和当前源码笼统地标为一个永不变化的 seed。

Genesis `core0` 可以、也必须承担第一次语义化 `core1` 的解释和物化；但在 `core1`
通过 gate 并晋升后，活动执行链必须变成 `core1 -> core2`。原始手写 `core0` 不需要
永久留在活动 TCB 中，只能作为归档、复验或回滚材料保留。

特别地，完成物化的 `core1` 在独立运行和解释 `EmptyBusiness` 时：

- 不得加载、调用或委托回 `core0`；
- 不得从父仓库取得 `core0` 的 runtime closure；
- 不得把 `core0` 的函数指针包装为所谓的 candidate；
- 只能依赖永久 Host TCB 和自身声明的 artifact closure。

因此这里的 fixed point 是可复验的关系和证据，不是某个必须永久存在的 core 对象。

## 4. 不增加第四套业务前台

业务作者仍然只使用：

1. Effect System / CURDE；
2. AST；
3. Handler。

自举类型属于内部 maintenance surface，不是第四套 authoring facade。它只组合现有
前台值、TrustBase 身份和 gate evidence。

### 4.1 三层可见性规则

“业务前台不出现 core”与“自举依赖必须显式”并不冲突。显式性按边界分层：

| 边界 | core 的可见性 |
|---|---|
| 业务作者前台 | 不出现 core 参数或 TrustBase 关键字 |
| 已发布 SDK | 默认绑定该 SDK 对应的 approved core，用户调用保持隐式 |
| SDK artifact/lock/manifest | 必须显式记录 core identity、schema 和 digest |
| maintenance/runtime | 必须显式传入 `BoundTrustBase` |

原则是：**对业务用户隐含，对产物显式，对内部绑定显式。**

SDK 不能通过“使用当前目录里碰巧存在的 core”实现这种隐含。它必须携带可审计锁：

```haskell
data SdkCoreLock = SdkCoreLock
  { sdkCoreRef           :: TrustBaseRef
  , sdkSurfaceDigest     :: Digest
  , sdkLoweringDigest    :: Digest
  }
```

用户入口和内部入口可以形成以下关系：

```haskell
runApp :: App -> IO RunResult
runApp app = do
  core <- bindEmbeddedSdkCore sdkCoreLock
  runAppWithCore core app

runAppWithCore :: BoundTrustBase -> App -> IO RunResult
```

`runAppWithCore` 只属于 maintenance/test/runtime surface，不是新的业务 facade。SDK
升级即意味着显式更新 `SdkCoreLock`；若 lock、artifact 或 manifest 不一致，必须在业务
AST 求值前失败。

### 4.2 可序列化的 TrustBase 引用

```haskell
data CoreId = CoreId String

data Digest = Digest String

data TrustBaseRef = TrustBaseRef
  { trustBaseCoreId          :: CoreId
  , trustBaseArtifactDigest  :: Digest
  , trustBaseManifestDigest  :: Digest
  , trustBaseSchemaVersion   :: Int
  , trustBaseKernelClaims    :: [ClaimName]
  }
```

`TrustBaseRef` 可以进入 manifest、BootstrapPlan 和 PromotionRecord。它只描述身份与承诺，
不包含函数、句柄、文件描述符或运行时 environment。

### 4.3 语义外的运行能力

```haskell
data BoundTrustBase where
  BoundTrustBase ::
    runtime ->
    (runtime -> FrameworkAsBusiness -> EmptyBusiness -> IO BootstrapEvidence) ->
    TrustBaseRef ->
    BoundTrustBase
```

`BoundTrustBase` 是 existential runtime capability：

- 不可序列化；
- 不进入用户 AST；
- 只能由 Host TCB 根据 `TrustBaseRef` 加载；
- 加载时必须验证 artifact 与 manifest digest；
- 是 self-bootstrap runner 的显式参数，不能从全局变量或父仓库隐式取得。
- 不能进入 SDK 的可序列化业务配置，只能由锁定的 `TrustBaseRef` 在运行边界绑定。

绑定入口：

```haskell
bindTrustBase ::
  HostCoreRegistry ->
  TrustBaseRef ->
  IO (Either TrustBaseBindingError BoundTrustBase)
```

任何 identity、schema 或 digest 不匹配都必须在 candidate 求值前失败。

## 5. FrameworkAsBusiness

当前 `MyFramework.Self.Model` 中的 `createUser/readUser/updateUser/deleteUser/emitAudit`
只适合作为 CURDE 语义 fixture。它应被明确命名为 fixture，不能继续承担“framework 已表达
自身”的证明。

真正的 `FrameworkAsBusiness` 必须通过正常三前台表达 framework core 的最小承诺：

```text
Effect System / CURDE
  validate facade
  validate single-input CURDE
  lower declarations
  compile cata control plan
  bind handlers
  execute runtime
  collect evidence
  materialize candidate artifact

AST
  candidate core 的 boot/control 关系

Handler
  由 BoundTrustBase 提供的上一代已编译实现
```

内部类型可以是：

```haskell
data FrameworkAsBusiness = FrameworkAsBusiness
  { frameworkCoreId          :: CoreId
  , frameworkEffectSystems   :: [EffectSystemDecl]
  , frameworkAst             :: AppBlueprintSeed
  , frameworkHandlerContract :: HandlerContractManifest
  , frameworkSchemaCatalog   :: SchemaCatalog
  , frameworkClaimCatalog    :: ClaimCatalog
  }
```

其中所有 record 字段必须可序列化。实际 Handler 闭包仍然只在 runtime binding 阶段进入。

候选模型至少要覆盖：

- CURDE C/U/R/D/E 语义；
- input 与 R value dependency；
- Implementation 的 lexical binding；
- 13 个 AST control constructors；
- `Fix + cata` 控制编译；
- Handler 输入使用约束；
- runtime 状态、observation、failure、cancellation、diagnosis；
- artifact materialization；
- TrustBase、schema 与 witness claim catalog。

“源码文件存在”不能作为 framework-as-business 的充分证据；这些能力必须有正常前台
handle、AST 位置和可执行 witness。

## 6. EmptyBusiness：递归基例

`EmptyBusiness` 必须是一个真实的、可序列化的 Unit 业务参数：

```haskell
data EmptyBusiness = EmptyBusiness
  deriving (Eq, Ord, Read, Show)
```

由于当前 `AstF` 没有 Empty 节点，不能为了自举方便伪造一个 CRUD 操作，也不应把
Stage2 冒充 terminal business。

解决方式是在 App/Blueprint 边界增加 nullary case，而不是增加新的控制节点：

```haskell
data AppSeed
  = EmptyAppSeed
  | BootAppSeed AstBlueprintSeed
```

约束：

- `EmptyAppSeed` 只表示零操作 App；
- 不 lower 为 `AstF`；
- 不产生 `ControlNode`；
- runtime 直接返回初始完成状态；
- 没有 EffectSystem、CURDE handle、Implementation、Handler、artifact 或 host IO；
- witness 可以产生 `EmptyBusinessClosed` evidence，但不能把它伪装成 runtime Fact。

这样 `AstF` 的 13 个控制节点保持不变，空业务只是 App 的代数基例。

完整关系是：

```haskell
runSelfBootstrap ::
  BoundTrustBase ->
  BootstrapRound ->
  IO (Either BootstrapFailure BootstrapEvidence)

data BootstrapRound = BootstrapRound
  { bootstrapPreviousCore     :: TrustBaseRef
  , bootstrapCandidateCore    :: FrameworkAsBusiness
  , bootstrapTerminalBusiness :: EmptyBusiness
  }
```

## 7. Self-interpret 与 exchangeability

必须分别观察 previous core 和 candidate core，而不是让两边都重新读取同一个
`SelfModel`。

```text
Stage Core0:
  BoundTrustBase 执行 candidate FrameworkAsBusiness

Stage Core1:
  candidate artifact 独立构建
  candidate 执行 EmptyBusiness

Semantic comparison:
  normalize(core0 observes candidate)
    ==
  normalize(core1 observes itself/terminal)
```

Stage Core0 可以帮助构建和验证 candidate；这不等于允许已物化的 candidate 在运行时
回调 Core0。独立性必须由 artifact closure、加载轨迹和负例共同证明：

```text
runtimeDependencies(core1) does not contain core0
```

规范化证据至少包含：

- facade surface digest；
- CURDE declaration/lowering digest；
- AST control-plan digest；
- schema catalog digest；
- claim catalog digest；
- handler contract digest；
- runtime semantic trace digest；
- failure/diagnosis policy digest；
- artifact source closure digest；
- TrustBaseRef identity；
- EmptyBusiness closure result。

以下内容不得进入 semantic equality：

- 临时路径；
- 时间戳；
- PID；
- build directory；
-日志顺序中没有语义的并发噪声。

AST 子节点顺序和声明中具有语义的 authoring order 必须保留。

## 8. 不可伪造的 witness

新增 `core-self-interpret-witness`，至少包含下列 claim：

```text
previous-core-runs-candidate
candidate-is-expressed-by-normal-facade
candidate-runs-as-framework-business
empty-business-closes-recursion
empty-business-has-no-curde
empty-business-has-no-handler
empty-business-has-no-host-io
trustbase-binding-is-explicit
trustbase-digest-matches
trustbase-not-forwarded-to-terminal-business
core0-core1-exchangeable
candidate-has-no-previous-core-runtime-dependency
host-kernel-capability-set-is-inclusion-minimal
semantic-fixed-point-passed
artifact-fixed-point-passed
self-bootstrap-claim-manifest-complete
```

`StageEvidence` 必须从实际运行结果收集。调用者不能构造一个带任意 digest 的
`StageEvidence`。

负例至少包括：

- candidate 仍引用 CRUD fixture 而不是 framework capability；
- 缺失 EmptyBusiness；
- EmptyBusiness 注册任何 handle、Implementation 或 Handler；
- candidate 直接访问父仓库或全局 runtime；
- `TrustBaseRef` 与加载 artifact digest 不匹配；
- candidate evidence 与 previous-core evidence 不同；
- SDK lock、manifest 与实际绑定的 core 不一致；
- `core1` 独立执行时加载或调用 `core0`；
- 移除一个声称必要的 HostKernel capability 后全部义务仍然通过；
- candidate 可以构建但无法执行 EmptyBusiness；
- artifact fixed point 通过但 semantic fixed point 失败；
- semantic fixed point 通过但 artifact fixed point 失败；
- claim 缺失、重复、为空或顺序漂移；
- promotion target 不是被本轮证据验证的 candidate digest。

## 9. Promotion 与 TrustBase 替换

自举验证与替换必须分开。

### 9.1 PromotionRecord

```haskell
data PromotionRecord = PromotionRecord
  { promotionPreviousCore       :: TrustBaseRef
  , promotionCandidateCore      :: TrustBaseRef
  , promotionSemanticEvidence   :: Digest
  , promotionArtifactEvidence   :: Digest
  , promotionEmptyBusinessProof :: Digest
  , promotionDecision           :: PromotionDecision
  }

data PromotionDecision
  = PromotionPending
  | PromotionApproved
  | PromotionRejected
```

### 9.2 Gate 顺序

```text
1. build
2. facade/CURDE/AST/Handler witnesses
3. previous core interprets candidate
4. candidate artifact independently builds
5. candidate interprets EmptyBusiness
6. core0/core1 semantic exchangeability
7. candidate has no runtime dependency or back-reference to previous core
8. Stage1/Stage2 artifact fixed point
9. HostKernel capability sufficiency and remove-one negative witnesses
10. TrustBase, SDK lock and schema manifests
11. scoped git diff
12. explicit maintainer promotion decision
```

任何失败、timeout、缺失证据都必须保持 previous core。

### 9.3 原子替换

仓库应保存一个小型、可审阅的当前指针：

```text
trustbase/current.json
```

内容只包含已晋升 `TrustBaseRef`。替换建议保持两个提交：

```text
commit 1: introduce candidate + evidence + PromotionRecord(Pending)
commit 2: switch current.json + PromotionRecord(Approved)
```

上一代 artifact 与 manifest 至少保留一个 release window，用于回滚和复验。

归档上一代不等于继续依赖上一代。晋升完成后，活动 core 的 dependency closure 中不得
包含上一代 core；回滚只能通过显式切换 `current.json` 完成。

Promotion 后的下一轮：

```text
previous = candidate from last approved PromotionRecord
candidate = newly authored FrameworkAsBusiness
terminal = EmptyBusiness
```

## 10. 模块落点

保留现有三前台，新增内部模块：

```text
MyFramework.Self.Fixture
  当前 CRUD SelfModel；只承担 CURDE coverage。

MyFramework.Self.CoreModel
  FrameworkAsBusiness 的可序列化表达。

MyFramework.Self.EmptyBusiness
  EmptyBusiness 与 EmptyAppSeed。

MyFramework.TrustBase.Core
  CoreId、TrustBaseRef、BoundTrustBase、bindTrustBase。

MyFramework.TrustBase.SelfInterpret
  BootstrapRound 与三层运行。

MyFramework.TrustBase.Promotion
  PromotionRecord、current.json 校验和显式替换工具。

app/CoreSelfInterpretWitness.hs
  语义闭包 witness。

app/CorePromotionTool.hs
  只生成/验证 promotion record；默认不修改 current pointer。
```

现有模块调整：

```text
MyFramework.Self.Model
  不再被描述为完整 framework self-model。

MyFramework.TrustBase.FixedPoint
  分离 SemanticCoreEvidence 与 ArtifactStageEvidence。

MyFramework.Self.Artifact
  manifest 记录 previous/candidate CoreId 和 TrustBaseRef digest。

scripts/check-release.ps1
  ordinary gate 不自动 promotion。

scripts/self-artifact.ps1
  在 semantic self-interpret precondition 通过后运行。
```

## 11. 实施顺序

### Phase A：纠正命名与证据边界

- 把当前 `SelfModel` 标记为 CURDE coverage fixture；
- 把当前 Stage0/1/2 报告标记为 artifact stages；
- 禁止 artifact fixed point 单独产生 semantic-self-bootstrap claim。

完成后状态仍是 `promotion blocked`。

### Phase B：显式 TrustBase 参数

- 实现 `CoreId`、`TrustBaseRef`、`BoundTrustBase`；
- 将 runtime/materializer 的隐式入口改为 self-bootstrap runner 参数；
- manifest、artifact 和 `SdkCoreLock` 写入实际使用的 TrustBaseRef digest；
- 保持业务 `runApp` 无 core 参数，增加内部 `runAppWithCore`；
- 增加 mismatch 与父仓库隐式读取负例。

### Phase B2：HostKernel 收缩

- 固定闭合 capability set 与顶层 `runSelfBootstrap`；
- 删除 AST 通用 host escape；
- 为每个 capability 增加 remove-one negative witness；
- 记录模型、义务集合和相对最小性证明版本。

### Phase C：FrameworkAsBusiness

- 用正常 CURDE/AST/Handler 表达框架核心能力；
- handler 由 BoundTrustBase 绑定；
- 建立独立 schema/claim catalog；
- 当前 CRUD fixture 不参与 core identity。

### Phase D：EmptyBusiness

- 在 AppSeed 层增加 `EmptyAppSeed`；
- 不增加 `AstF`/`ControlNode`；
- 证明无 CURDE、无 Handler、无 host IO；
- candidate core 必须实际接受并完成 EmptyBusiness。

### Phase E：双固定点

- core0/core1 semantic exchangeability；
- artifact1/artifact2 reproducibility；
- candidate 独立运行时对 previous core 零回指；
- 任一失败都阻断 promotion。

### Phase F：Promotion replacement

- 实现 PromotionRecord；
- 引入 `trustbase/current.json`；
- 提供只读验证和显式 approve 两种命令；
- 保留 previous artifact 回滚路径。

### Phase G：SDK CI/CD

只有 promotion 后的 core 可以成为 `sdk-lower` 输入。CI 顺序应为：

```text
approved TrustBase core
  -> sdk-lower
  -> generated SDK source manifest
  -> independent SDK build/witness
  -> publish artifact or update dsl-sdk
```

不得从一个只有 artifact fixed point、尚未通过 semantic closure 的 worktree 发布 SDK。

## 12. 完成标准

全部满足后，才可以把项目称为“可用的语义自举 framework core”：

- framework 自身由正常 CURDE/AST/Handler 前台表达；
- previous core identity 与 runtime 作为显式参数进入；
- candidate core 完成 EmptyBusiness；
- EmptyBusiness 无 CURDE、Handler、artifact 和 host IO；
- core0/core1 semantic diffs 为 0；
- Stage1/Stage2 artifact diffs 为 0；
- candidate 的 runtime dependency closure 不包含 previous core；
- TrustBaseRef 与实际 artifact/manifest digest 一致；
- SDK 的 `SdkCoreLock` 与实际绑定的 approved core 一致；
- Host TCB 与 rotating framework core 分离；
- HostKernel 在声明的模型和义务下充分且包含最小；
- HostKernel 不含 framework 或业务操作语义；
- promotion record 指向本轮实际验证的 candidate；
- replacement 是显式决定，失败时 previous core 保持不变；
- 下一轮能以晋升后的 core 作为 previous core 重复同一流程；
- SDK 只从 approved core 生成。

## 13. 冻结约束

本设计不改变以下原则：

- 用户前台仍然只有 Effect System、AST、Handler；
- 业务前台不暴露 core 参数；
- SDK 对业务用户隐式绑定 approved core，但其 artifact/lock/manifest 必须显式记录该 core；
- maintenance/runtime 显式接收 `BoundTrustBase`；
- `core0` 可以解释和物化 `core1`，但晋升后的 `core1` 不得回调 `core0`；
- 永久外部边界是最小 Host TCB，Previous Framework Core 必须逐轮轮换；
- Build TCB、Runtime HostKernel 与 Previous Framework Core 是三个不同边界；
- Runtime HostKernel 只主张相对于已声明模型和义务的包含最小性，不主张全局绝对最小；
- `implementation` 只补齐数据和外部参数；
- C/U/D/E 的公开结果仍然是状态；
- R 仍然提供可序列化数据；
- 每个 Fact 最多一个显式 input；
- AST 仍然只表达 boot/control flow；
- `Fix + cata` 仍是当前 recursion-scheme 核；
- 不引入 `ana`、`hylo` 或 JSON-RPC；
- EmptyBusiness 不增加新的 `AstF` 控制节点；
- promotion 与普通构建、普通业务执行严格分离。
