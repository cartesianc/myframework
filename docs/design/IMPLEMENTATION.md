# 实现对应与后续工作

[目录](README.md)中的文件定义最新目标设计，阅读架构时以文档为准。发布代码采用早期试验架构，本次发布范围为文档；源码和 core 记录保持各自的版本。

## 1. 区分发布代码与本地试验参考

| 层次 | 状态 |
|---|---|
| 最新设计 | 本目录描述的目标架构与接口，优先阅读 |
| 当前远端代码 | 本次文档发布基于 303ae9c，包版本为 0.1.0.0；源码保持原样 |
| 本地试验参考 | 355d7d0，包版本为 0.2.0.0，包含 CURDE/v5.2 探索，保留在本地开发历史 |

下表记录本地试验代码可供后续复用的基础。路径定位上述本地参考版本，阅读发布代码时应使用对应版本的文件清单。

| 源码 | 可以复用的基础 | 目标差异 |
|---|---|---|
| src/MyFramework/Effect.hs | EffectRef、EffectSpec、EffectIR | 该版采用 C/U/R/D/E/MAP，目标采用新分类与统一 Free 前台 |
| src/MyFramework/Handler/Adapter.hs | 宿主函数与 codec 适配 | 该版为 input → IO (Either String output)，目标 impl 通过具体 effect 的类方法获得依赖 |
| src/MyFramework/Graph.hs | AST/Input/Value 三种关系 | 需按句柄组合重新核对 input 的解释 |
| src/MyFramework/Ast.hs、Recursion.hs | Fix、cata 与控制构造 | 该版以 Fix/cata 为基础，ana/hylo 保留为扩展方向 |
| src/MyFramework/Runtime/V52.hs | 执行身份、结果、调度、清理 | 需衔接 handler 字典、bind 调用、资源协议与模块后端 |
| src/MyFramework/Hook.hs、Callback.hs、Loop.hs | 事件、回调、监听策略 | 需与新 scope/once 对齐 |
| src/MyFramework/Schema.hs、Elaboration.hs | 参数契约、解码与验证 | 可承接配置和插件装配 |

上述本地参考提供 cleanup 和去重机制。目标 once 的线性所有权接口、具体 effect 的存在封装与 handler 字典传递、统一注册及 listen/mock/stress/log/show 后端列入后续实现工作。

## 2. 建议实现顺序

| 阶段 | 交付内容 | 最小验证 |
|---|---|---|
| 1. 普通调用 | EffectF GADT、effect 函数、公开 Effect record、handler 字典、bind 绑定的 impl | 合并入口对应内部 Free 构造；impl 取得对应依赖，各执行的参数与身份独立 |
| 2. 值准备 | Read、Map、require、结果与等待协议 | 两路值汇入单一参数；input 仍只有一个 |
| 3. 资源与错误 | scope、layer、线性关闭、X 范围 | 正常、失败、取消均按协议处理责任，错误按 scope 路由 |
| 4. 控制与事件 | AST、loop、Hanging、执行共享 | 只执行选中分支，同次请求共享，新轮次重新执行 |
| 5. 模块观测 | 模块身份、事件、快照、log/show | 可关联等待与来源，观察读取事件和快照 |
| 6. mock/stress | 场景绑定与负载调度 | handler 解释与 impl 选择可追踪，并发调用身份独立，获取/释放配套 |
| 7. 配置启动 | 契约 codec、稳定引用、插件标识 | 缺失实现和解码失败在业务启动前报告 |

资源阶段还需覆盖：并发使用中取消、关闭失败、scope 退出后的回调、旧资源引用失效。运行时需要原子的执行去重与明确的死锁/等待诊断。

按改动选择类型检查、资源使用负例和运行测试。现有 witness 仅证明其实际覆盖的试验版合同。

## 3. 后续接口细节

这些内容保留思考空间，按已经确定的文件职责继续设计：

- 按 [CONSTRUCTION.md](CONSTRUCTION.md) 扩展完整 record 字段、分类专用内容及默认值。
- handlers 取得类方法字典的原生 Haskell 接口。
- 一个 effect 多组 handler 方法和多个 impl 的具体绑定形式。
- 存在包、内部 EffectIO 调用环境及 impl 返回 effect 的封装接口。
- 多 scope 要求、借用接口、关闭失败处理。
- 参数及流式结果的就绪粒度。
- parallel 失败传播、取消策略；loop 变化判定、重入与合并。
- 嵌套错误范围、恢复与重试协议、针对单个 effect 的 help。
- 动态 hook、外部工具协议、配置与插件版本兼容。

内部通过 GADT、associated families 和存在封装组织具体 effect 及其类型类字典；前台保持统一 record、handler 方法和 bind 绑定。input 与 impl 的连接以注册的显式绑定为依据。

## 4. 运行已发布的旧试验代码

```powershell
stack --work-dir .stack-work-codex build
stack --work-dir .stack-work-codex exec curde-semantics-witness
stack --work-dir .stack-work-codex exec curde-runtime-witness
```

crawler-facade-witness、schema-elaboration-witness、effect-application-witness、facade-v52-witness 和 semantic-closure-witness 适用于上述本地试验参考版本。

AST 执行边界与普通发布检查分别使用：

```powershell
.\scripts\check-ast-execution-boundary.ps1
.\scripts\check-release.ps1
```

这些命令运行旧试验实现，目标 API 的落地情况由新的实现记录说明。本次验证覆盖文档链接、示例关系、术语和格式。

## 5. 既有产物维护边界

自举、TrustBase 和 SDK 属于现有实现的维护体系，与业务注册接口分开：

- 构建工具链、封闭 HostKernel、可轮换 framework core 是三个不同信任边界。
- TrustBaseRef 保存可序列化的身份与摘要；BoundTrustBase 是运行时存在封装，由 HostKernel 核对并加载。
- SDK materialization 需要匹配的 core manifest、已批准 promotion 和 current pointer，并验证生成文件摘要。
- 语义自解释与产物固定点是独立验证义务；每轮记录对应其自身源码，新设计需要对应的实现与验证。
- 普通构建和验证生成相应证据；current core 的批准与切换属于单独的维护动作。

现有产物身份、历史与证据继续保存在 [trustbase](../../trustbase/README.md)，本次文档整理保留其原有内容。当前目标设计统一由本目录描述。
