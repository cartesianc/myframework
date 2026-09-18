# 设计目录

这是 myframework 当前目标设计的唯一入口。设计整理日期：2026-09-18。

**文档与当前发布代码不一致，请优先阅读本目录。** 本目录描述最新架构和目标接口；已发布代码仍是旧试验版。本次仅发布文档，不同步本地尚未发布的源码和 core 记录。

Effect 是可读的操作中间表示（IR）：用统一 record 声明操作、依赖和实现绑定，用 AST 声明模块控制流，再由 interpreter 驱动。解释器与运行时共同提供 VM 式模块后端，使同一份声明可用于执行、监听、mock、stress 和 log/show。

本文档中的接口是目标写法。已发布代码、本地试验参考与后续实现计划的区别，集中说明在 [IMPLEMENTATION.md](IMPLEMENTATION.md)。

## 文件分工

```text
docs/design/
├── README.md          目录、文件关系、总约定
├── QUICKSTART.md      简版：有什么功能、怎么用
├── EFFECT.md          注册 record、分类、身份、Free 表示
├── IMPL.md            类型类句柄、instance、绑定函数
├── AST.md             模块控制流、read/map/require 参数表达式
├── HANGING.md         回调、等待、监听与恢复
├── RESOURCE.md        layer、scope、线性 once、错误范围
├── INTERPRETER.md     执行、模块观测、mock/stress、配置启动
└── IMPLEMENTATION.md  源码对应、实现顺序、尚未定稿的细节
```

只想使用框架，读 [QUICKSTART.md](QUICKSTART.md)。完整设计按以下文件关系阅读；每个主题只在对应文件中定义。

| 文件 | 与其他文件的关系 |
|---|---|
| [EFFECT.md](EFFECT.md) | 定义公共 IR；其中 handler 指向 IMPL，scope/layer/once/help 的语义由 RESOURCE 定义 |
| [IMPL.md](IMPL.md) | 解释 EFFECT 的绑定；消费 AST 准备的参数，使用 RESOURCE 提供的能力 |
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

前台使用 Haskell record、函数和 instance。顺序固定为 **注册并绑定 impl → 提供句柄 instance → 编写 impl**。不同注册共享统一描述类型；显式绑定确定连接，分类能力和后端实例提供多态。

入口保持：

```haskell
main = interpreter ast effect
```

这里的 effect 是注册集合；单条注册使用的 effect 构造函数位于注册模块。Hanging 随程序描述装配。main 只负责启动。
