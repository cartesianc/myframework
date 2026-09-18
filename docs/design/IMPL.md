# Handler 与 impl：句柄解释和依赖注入

本文件定义 [EFFECT.md](EFFECT.md) 中 handler 与 bind 的语义，以及类型类、存在封装和 impl 的配合方式。执行调度见 [INTERPRETER.md](INTERPRETER.md)。

## 1. 三个角色

| 名称 | 定义 |
|---|---|
| handler / 句柄 | Effect 能力类型类提供的类方法，例如 execute、error_handler、资源获取和关闭算子 |
| instance | 为具体 effect 提供类方法的 ad-hoc 多态解释 |
| impl | 注册通过 bind 绑定的实现函数；外部操作以原子 IO 为执行单位，函数体使用句柄获得依赖 |

具体 effect 通过存在封装保存其内部表示和类型类字典。impl 调用类方法时，使用该 effect 的实例解释完成依赖注入。原子 IO 表示框架调度的一次最小外部操作，异常与资源清理由相应协议管理。Pure、Map 使用各自的纯计算能力。

## 2. 用户写法：注册 → instance → impl

以下为目标 API 伪代码。LoadText、ReportLoadError 是插件提供的具体 effect 表示；loadText、reportLoadError 是业务注册值。注册的公共类型统一为 EffectChain。

### 注册时填写 handler 并绑定 impl

沿用[简版](QUICKSTART.md)的 loadText，注册一个处理文件 scope 错误的 effect：

```haskell
reportLoadError :: EffectChain
reportLoadError = effect $ Effect
  { typing  = X
  , input   = loadText
  , require = noArgs
  , handler = handlers @ReportLoadError
  , bind    = reportImpl
  , once    = False
  , scope   = noScope
  , layer   = noLayer
  , result  = unitResult
  , help    = scopeOf fileSession
  }
```

handlers @ReportLoadError 表示取得该具体 effect 的类方法集合；bind 保存 reportImpl。help 的错误范围见 [RESOURCE.md](RESOURCE.md)。

### instance 解释类方法

```haskell
instance Effect 'Excu LoadText
instance Execution LoadText where
  execute upstream = do
    LoadText context <- currentEffect
    usingFile context upstream readTextIO

instance Effect 'X ReportLoadError
instance ErrorHandling ReportLoadError where
  error_handler upstream = do
    ReportLoadError context <- currentEffect
    withinErrorScope context upstream writeErrorIO
```

execute 和 error_handler 是 handler。instance 为对应 effect 解释算子；模式匹配取得具体表示中的上下文，句柄解释使用其中的参数与能力。

这里的 readTextIO、writeErrorIO 是宿主 IO 动作，usingFile、withinErrorScope 代表相应的框架操作适配。资源 effect 可以通过同一能力接口提供获取、借用、关闭等多组类方法。

### impl 调用类方法

```haskell
loadImpl :: Impl LoadText
loadImpl upstream =
  execute upstream

reportImpl :: Impl ReportLoadError
reportImpl upstream =
  error_handler upstream
```

函数等号右侧的类方法调用就是句柄使用点。具体签名确定目标 effect 的解释，框架随调用提供实例字典和执行上下文。

loadImpl 的业务连接是 fileSession → loadText，reportImpl 的连接是 loadText → reportLoadError。输入来自注册的 input，输出归属当前目标的同一次执行；业务结果另存于运行时，由 Read 提供给消费者。

## 3. 框架原理：类型类字典与存在封装

框架内部可以用下面的类型关系承接上述写法：

```haskell
class Effect (typing :: EffectTyping) e

class Effect 'Excu e => Execution e where
  execute :: EffectChain -> EffectIO e e

class Effect 'X e => ErrorHandling e where
  error_handler :: EffectChain -> EffectIO e e

type Impl e = EffectChain -> EffectIO e e
```

EffectIO 是内部的一次调用环境：承载当前目标、已准备参数、执行身份与受控 scope 使用入口，并由解释器运行其中的 IO。这里给出语义形状，具体封装 API 属于实现工作。

“自动依赖注入”由以下步骤实现：

1. GHC 根据具体 effect 表示的类型确定类方法实例。
2. 注册封装保存具体 effect 表示、所需能力字典和已绑定 impl。
3. 解释器打开存在包，建立本次调用环境并运行 impl。
4. impl 中的类方法调用使用已确定的实例解释，访问声明允许的依赖。
5. 调用返回本次目标 effect，解释器重新封装并记录状态与结果。

对外的注册集合统一保存存在包；包内保留具体类型、字典和 impl 的对应关系。配置变化可以复用同一具体表示与 instance，运行差异由注册身份和调用数据表达。

存在封装在模式匹配时恢复可用约束，具体表示的模式匹配随后在该作用域内进行。这与 GHC 的[存在类型与类型类字典](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html#existentials-and-type-classes)机制对应。

## 4. 实现职责

- GADT、associated data family 可以组织分类专用内容，associated type family 可以关联参数与能力类型。
- 前台保留统一 record、普通函数和 instance；内部封装承接类型关系。
- handler 集合与 bind 的 impl 保持对应，调用环境按执行身份提供参数和能力。
- 线性所有权由专用资源接口保存、转移和消耗；共享 IR 保存描述和引用。
- X 的错误记录、恢复、转换和重试各有明确结果；具体协议由错误处理声明确定。
