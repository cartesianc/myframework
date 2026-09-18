# 资源、作用域与错误范围

本文件定义 [EFFECT.md](EFFECT.md) 中 layer、scope、once、help 的共同语义。impl 通过 [handler 类方法](IMPL.md)取得资源能力，[INTERPRETER.md](INTERPRETER.md) 管理生命周期，[HANGING.md](HANGING.md) 遵守有效期。

## 1. 三个资源字段

| 字段 | 回答的问题 |
|---|---|
| layer | 当前操作需要哪个 scope 提供什么能力？ |
| scope | 当前资源拥有者用哪个 Close effect 结束作用域？ |
| once | 哪个操作或资源责任必须遵守线性使用协议？ |

layer 关联实际 effect 引用，基线形式为 need fileSession，表达当前 effect 对 scope 能力的要求。多个 scope 要求的组合 ADT 属于后续接口设计。

资源获取所需的文件路径等业务值由 require/Read/Map 提供，句柄在本次调用环境中使用这些值。

## 2. 开关归同一个资源拥有者

以下 handler 列列出类型类方法，bind 列列出绑定函数：

| 注册 | typing | input | handler 句柄 | bind | scope | layer | once |
|---|---|---|---|---|---|---|---|
| fileSession | Excu | end | acquire、release | openFileImpl | closeFile | 无 | False |
| readFile | Excu | fileSession | read_text | readFileImpl | 无 | need fileSession | False |
| writeFile | Excu | fileSession | write_text | writeFileImpl | 无 | need fileSession | False |
| closeFile | Close | fileSession | close_scope | closeFileImpl | 无 | fileSession 的清理能力 | True |

具体 effect 的 instance 提供这些方法的解释，注册 record 通过 handlers 取得相应方法集合。impl 调用类方法时获得其参数和受控资源能力。

fileSession 的能力接口包含获取和释放句柄。closeFile 的 close_scope 解释使用拥有者的释放能力，消费对应责任。读写通过 layer 使用已打开的 scope，顺序由 AST 决定。

closeFile 的 input 始终指向 fileSession。scope → closeFile 表达退出时的清理关联，清理使用已持有的资源。

## 3. once 是线性责任

once 的布尔字段声明线性要求，资源插件和运行器通过线性接口保存、转移和消耗责任。运行时去重另按执行身份管理调用共享。

成功获取资源后产生唯一的所有权或关闭凭证；关闭消耗该凭证。打开与关闭方法保持既定类型，调用过程创建并移交相应凭证。

实现遵循以下规则：

- 线性拥有者由专用接口管理，Free 描述、共享调用元数据和普通存在包保存受控引用。
- 转移、借用和消耗经过受控接口，使用权受 scope 限制。
- scope 退出先等待或取消使用者，使监听失效，再执行关闭。
- 正常结束、失败和取消均由运行时调度清理协议，线性接口约束责任的使用。
- 关闭后旧使用权失效，关闭失败进入明确的记录与处理路径。

类型机制参考 [GHC LinearTypes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html)。异常清理协议参见 [Linear types and exceptions](https://www.tweag.io/blog/2020-02-19-linear-type-exception/)。

## 4. X 按 scope 处理错误

X 注册通过 help 指定处理范围：

```haskell
help = scopeOf fileSession
```

error_handler 是 X 分类的类方法，instance 为具体错误处理 effect 提供解释，bind 指定调用该句柄的 impl。

错误事件携带来源注册、执行身份、scope 身份和错误值。后端按这些身份匹配处理范围，并将相应错误交给句柄。

X 可以消费失败上游的错误，Close 在 scope 清理阶段执行；解释器按各分类的启动条件调度。

记录错误、恢复业务、转换失败和重试各有明确结果。嵌套 scope 的处理优先级、未处理错误传播、重试语义，以及针对单个 effect 的 help 扩展属于后续协议设计。
