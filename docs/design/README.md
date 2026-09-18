# 设计目录

这是 myframework 当前目标设计的唯一入口。设计整理日期：2026-09-18。

**请优先阅读本目录理解最新设计。** 本目录描述目标架构和接口，已发布代码保留早期试验实现。本次发布范围为文档，源码和 core 记录保持各自的版本。

Effect 是可读的操作中间表示（IR）：用统一 record 声明操作和依赖，以 handler 引用具体 effect 的类型类句柄，以 bind 绑定 impl，用 AST 声明模块控制流，再由 interpreter 驱动。解释器与运行时共同提供 VM 式模块后端，使同一份声明可用于执行、监听、mock、stress 和 log/show。

本文档中的接口是目标写法。已发布代码、本地试验参考与后续实现计划的区别，集中说明在 [IMPLEMENTATION.md](IMPLEMENTATION.md)。

## 文件分工

```text
docs/design/
├── README.md          目录、文件关系、总约定
├── QUICKSTART.md      简版：有什么功能、怎么用
├── EFFECT.md          注册 record、分类、身份、Free 表示
├── IMPL.md            handler、instance、存在封装、impl 依赖注入
├── AST.md             模块控制流、read/map/require 参数表达式
├── HANGING.md         回调、等待、监听与恢复
├── RESOURCE.md        layer、scope、线性 once、错误范围
├── INTERPRETER.md     执行、模块观测、mock/stress、配置启动
└── IMPLEMENTATION.md  源码对应、实现顺序、尚未定稿的细节
```

只想使用框架，读 [QUICKSTART.md](QUICKSTART.md)。完整设计按以下文件关系阅读；每个主题只在对应文件中定义。

| 文件 | 与其他文件的关系 |
|---|---|
| [EFFECT.md](EFFECT.md) | 定义公共 IR；handler 与 bind 的配合见 IMPL，scope/layer/once/help 的语义见 RESOURCE |
| [IMPL.md](IMPL.md) | 定义类型类句柄、实例解释、存在封装和 impl；消费 AST 准备的参数及 RESOURCE 提供的能力 |
| [AST.md](AST.md) | 引用 EFFECT 构造节点，并规定主控制树 |
| [HANGING.md](HANGING.md) | 补充 AST 之外的事件入口，生命周期受 RESOURCE 约束 |
| [RESOURCE.md](RESOURCE.md) | 为 EFFECT、IMPL 和 HANGING 规定资源与错误边界 |
| [INTERPRETER.md](INTERPRETER.md) | 汇合上述文件，规定统一运行及外部工具接口 |
| [IMPLEMENTATION.md](IMPLEMENTATION.md) | 将上述目标映射到现有源码与后续工作 |

## 全局约定

框架有五个组成部分：Effect、Implementation、AST、Hanging、Interpreter。资源规则横跨这些部件，因此单独成文。

| 关系 | 由什么表达 |
|---|---|
| 从原子句柄组合出更大的操作 | 单一 input |
| 业务值从哪里来、怎样转换 | require、Read、Map、节点参数表达式 |
| 模块的顺序、并行、选择、监听 | AST 与 Hanging |
| 使用什么资源、由谁关闭 | layer、scope、线性 once |

前台使用 Haskell record、函数和 instance。顺序固定为 **注册 handler 并绑定 impl → 为具体 effect 提供 instance → impl 使用句柄**。

handler 是类型类方法，instance 为具体 effect 提供 ad-hoc 解释，bind 指定实现函数。存在封装保存具体 effect 及其能力字典；impl 使用类方法时，依赖随该字典与调用环境注入。注册集合共享统一描述类型。

入口保持：

```haskell
main = interpreter ast effect
```

这里的 effect 是注册集合；单条注册使用的 effect 构造函数位于注册模块。Hanging 随程序描述装配。main 只负责启动。
