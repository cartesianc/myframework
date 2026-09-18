# 实现对应与后续工作

[目录](README.md)中的文件定义最新目标设计，阅读架构时以文档为准。已发布代码与文档不一致；本次只更新文档，不发布本地源码和 core 记录。

## 1. 区分发布代码与本地试验参考

| 层次 | 状态 |
|---|---|
| 最新设计 | 本目录描述的目标架构与接口，优先阅读 |
| 当前远端代码 | 本次文档发布基于 303ae9c，包版本为 0.1.0.0；源码保持原样 |
| 本地试验参考 | 355d7d0，包版本为 0.2.0.0，包含 CURDE/v5.2 探索；尚未随本次文档发布 |

下表记录本地试验代码可供后续复用的基础。路径用于定位该参考版本，部分文件在远端旧版中尚不存在，因此不作为当前远端源码链接。

| 源码 | 可以复用的基础 | 目标差异 |
|---|---|---|
| src/MyFramework/Effect.hs | EffectRef、EffectSpec、EffectIR | 仍是 C/U/R/D/E/MAP，尚无新分类与统一 Free 前台 |
| src/MyFramework/Handler/Adapter.hs | 宿主函数与 codec 适配 | 该版为 input → IO (Either String output)，尚非新 Impl |
| src/MyFramework/Graph.hs | AST/Input/Value 三种关系 | 需按句柄组合重新核对 input 的解释 |
| src/MyFramework/Ast.hs、Recursion.hs | Fix、cata 与控制构造 | 该版没有 ana/hylo |
| src/MyFramework/Runtime/V52.hs | 执行身份、结果、调度、清理 | 需衔接新绑定、资源协议与模块后端 |
| src/MyFramework/Hook.hs、Callback.hs、Loop.hs | 事件、回调、监听策略 | 需与新 scope/once 对齐 |
| src/MyFramework/Schema.hs、Elaboration.hs | 参数契约、解码与验证 | 可承接配置和插件装配 |

上述本地参考中未发现 LinearTypes / %1 资源接口。现有 cleanup 和去重不构成 once 的线性所有权保证。完整的统一注册接口及 listen/mock/stress/log/show 后端仍待落地。

## 2. 建议实现顺序

| 阶段 | 交付内容 | 最小验证 |
|---|---|---|
| 1. 普通调用 | Free record、稳定身份、显式绑定、分类句柄、Impl | 两个注册不串参数或身份，链长变化不增加用户类型包装 |
| 2. 值准备 | Read、Map、require、结果与等待协议 | 两路值汇入单一参数；input 仍只有一个 |
| 3. 资源与错误 | scope、layer、线性关闭、X 范围 | 正常、失败、取消均按协议处理责任；错误不串 scope |
| 4. 控制与事件 | AST、loop、Hanging、执行共享 | 未选分支不执行；同次请求共享，新轮次重新执行 |
| 5. 模块观测 | 模块身份、事件、快照、log/show | 可关联等待与来源，观察不触发业务 |
| 6. mock/stress | 场景绑定与负载调度 | 实现选择可追踪；并发调用身份独立，获取/释放配套 |
| 7. 配置启动 | 契约 codec、稳定引用、插件标识 | 缺失实现和解码失败在业务启动前报告 |

资源阶段还需覆盖：并发使用中取消、关闭失败、scope 退出后的回调、旧资源引用失效。运行时需要原子的执行去重与明确的死锁/等待诊断。

按改动选择类型检查、资源使用负例和运行测试。现有 witness 仅证明其实际覆盖的试验版合同。

## 3. 尚未固定的接口细节

这些内容保留思考空间，不改变已经确定的文件职责：

- record 默认值、分类专用字段的原生 Haskell 接口。
- 一个 effect 多组操作和多个 impl 的具体绑定形式。
- 多 scope 要求、借用接口、关闭失败处理。
- 参数及流式结果的就绪粒度。
- parallel 失败传播、取消策略；loop 变化判定、重入与合并。
- 嵌套错误范围、恢复与重试协议、针对单个 effect 的 help。
- 动态 hook、外部工具协议、配置与插件版本兼容。

内部可用 GADT、associated families 和存在封装组织能力；前台保持统一 record 与显式绑定。没有为每条注册连接增加静态身份证明的实现任务。

## 4. 运行已发布的旧试验代码

```powershell
stack --work-dir .stack-work-codex build
stack --work-dir .stack-work-codex exec curde-semantics-witness
stack --work-dir .stack-work-codex exec curde-runtime-witness
```

crawler-facade-witness、schema-elaboration-witness、effect-application-witness、facade-v52-witness 和 semantic-closure-witness 属于上述本地试验参考，不作为当前远端版本的运行命令。

AST 执行边界与普通发布检查分别使用：

```powershell
.\scripts\check-ast-execution-boundary.ps1
.\scripts\check-release.ps1
```

这些命令运行旧试验实现，不代表目标 API 已实现。本次仅检查文档，不重新验证旧版运行结果或执行重型发布验证。

## 5. 既有产物维护边界

自举、TrustBase 和 SDK 属于现有实现的维护体系，与业务注册接口分开：

- 构建工具链、封闭 HostKernel、可轮换 framework core 是三个不同信任边界。
- TrustBaseRef 保存可序列化的身份与摘要；BoundTrustBase 是运行时存在封装，由 HostKernel 核对并加载。
- SDK materialization 需要匹配的 core manifest、已批准 promotion 和 current pointer，并验证生成文件摘要。
- 语义自解释与产物固定点是独立验证义务；旧轮次的通过记录不证明新设计完成。
- 普通构建和验证不切换 current core；产物批准与切换是单独的维护动作。

现有产物身份、历史与证据继续保存在 [trustbase](../../trustbase/README.md)。本次文档整理不修改它们，也不把旧合同继续保留为另一套目标设计。
