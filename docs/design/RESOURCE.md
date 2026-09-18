# 资源、作用域与错误范围

本文件定义 [EFFECT.md](EFFECT.md) 中 layer、scope、once、help 的共同语义。由 [IMPL.md](IMPL.md) 使用能力，[INTERPRETER.md](INTERPRETER.md) 管理生命周期，[HANGING.md](HANGING.md) 遵守有效期。

## 1. 三个资源字段

| 字段 | 回答的问题 |
|---|---|
| layer | 当前操作需要哪个 scope 提供什么能力？ |
| scope | 当前资源拥有者用哪个 Close effect 结束作用域？ |
| once | 哪个操作或资源责任必须遵守线性使用协议？ |

layer 关联实际 effect 引用，基线形式为 need fileSession。它是本框架的 scope 要求，不等同于某个外部库的完整 Layer 服务构造系统。多个 scope 要求的组合 ADT 尚待确定。

资源获取所需的文件路径等业务值仍由 require/Read/Map 提供，layer 不会凭空生成这些值。

## 2. 开关归同一个资源拥有者

以文件为例：

| 注册 | typing | input | handler | scope | layer | once |
|---|---|---|---|---|---|---|
| fileSession | Excu | end | openFileImpl | closeFile | 无 | False |
| readFile | Excu | fileSession | readFileImpl | 无 | need fileSession | False |
| writeFile | Excu | fileSession | writeFileImpl | 无 | need fileSession | False |
| closeFile | Close | fileSession | closeFileImpl | 无 | fileSession 的清理能力 | True |

fileSession 的实现载体提供打开和关闭等多组句柄。读写通过 layer 使用已打开的 scope；先读还是先写，由 AST 决定。

closeFile 始终关联 fileSession。最后执行写入不改变关闭操作的 input。

scope → closeFile 是清理关联，不是“打开前先关闭”的执行依赖。清理阶段解析已持有的资源，不能沿该关联重新获取资源或递归启动打开操作。

## 3. once 是线性责任

once 的布尔字段声明线性要求，真正的保证由资源插件和运行器的线性接口落实。它与运行时去重、single-flight 或“最多调用一次”的普通标志不同。

成功获取资源后产生唯一的所有权或关闭凭证；关闭消耗该凭证。函数本身不会在打开后改变类型，变化的是运行时是否持有这份责任。

实现必须满足：

- 线性拥有者不进入可任意复制的 Free 描述、普通 Binding、普通存在包或回调闭包。
- 转移、借用和消耗经过受控接口；使用权受 scope 限制。
- scope 退出先等待或取消使用者，并使监听失效，再执行关闭。
- 正常结束、失败和取消都进入清理协议；线性类型本身不会调度异常清理。
- 关闭后不能通过旧引用继续有效使用资源；关闭失败也必须明确记录和处理。

类型机制参考 [GHC LinearTypes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html)。异常安全需要额外执行协议，参见 [Linear types and exceptions](https://www.tweag.io/blog/2020-02-19-linear-type-exception/)。

## 4. X 按 scope 处理错误

X 注册通过 help 指定处理范围，基线写法：

```haskell
help = scopeOf fileSession
```

错误事件携带来源注册、执行身份、scope 身份和错误值。处理器只能接收符合其声明范围的事件；同名注册的其他调用不能因此混入。

X 可以消费失败上游的错误；Close 可以在 scope 的清理阶段执行。解释器不能给所有分类统一套用“上游成功且 scope 仍开放”的启动条件。

记录错误、恢复业务、转换失败和重试是不同结果，协议必须明确区分。嵌套 scope 的处理优先级、未处理错误传播、重试语义，以及针对单个 effect 的 help 扩展仍待确定。
