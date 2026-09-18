# Implementation：绑定、句柄与函数

本文件接续 [EFFECT.md](EFFECT.md)。注册字段中的 handler 绑定 impl；类型类方法提供 impl 使用的句柄算子。执行和业务结果记录由 [INTERPRETER.md](INTERPRETER.md) 完成。

## 1. 先注册并绑定函数

沿用[简版](QUICKSTART.md)中的 loadText，再声明处理文件 scope 错误的 effect：

```haskell
reportLoadError :: EffectChain
reportLoadError = effect $ Effect
  { typing  = X
  , input   = loadText
  , require = noArgs
  , handler = reportImpl
  , once    = False
  , scope   = noScope
  , layer   = noLayer
  , result  = unitResult
  , help    = scopeOf fileSession
  }
```

help 的范围由 [RESOURCE.md](RESOURCE.md) 定义。X 的分类专用字段由内部表示承接；以上是前台接口示意。

## 2. 类型类定义句柄，instance 给出解释

框架提供统一 Effect 能力体系，按分类暴露专用句柄。以下类名与签名展示关系，具体 API 待实现：

```haskell
class Effect backend

class Effect backend => Execution backend where
  execute :: backend -> Binding -> EffectChain -> EffectChain

class Effect backend => ErrorHandling backend where
  error_handler :: backend -> Binding -> EffectChain -> EffectChain
```

插件提供后端载体和实例：

```haskell
instance Effect FileBackend
instance Execution FileBackend where
  execute backend binding upstream =
    external backend binding upstream readTextIO

instance Effect ConsoleBackend
instance ErrorHandling ConsoleBackend where
  error_handler backend binding upstream =
    recover backend binding upstream writeErrorIO
```

execute、error_handler 是句柄；external、recover 是构造待执行操作的组合子。资源载体还可以提供获取、借用、关闭等多个方法。

实例选择针对 FileBackend 等类型。两个 EffectChain 值拥有不同 typing 或注册名称，不会自动获得两个 Haskell instance。注册通过显式绑定选择需要的实例；多态主要用于能力接口和可复用的后端解释。

## 3. impl 在等号右侧使用句柄

```haskell
type Impl = Binding -> EffectChain -> EffectChain

loadImpl :: Impl
loadImpl binding upstream =
  execute fileBackend binding upstream

reportImpl :: Impl
reportImpl binding upstream =
  error_handler consoleBackend binding upstream
```

binding 是框架提供的调用环境，此处显式列出，以说明目标和参数的来源：

| 内容 | 作用 |
|---|---|
| 当前目标及执行身份 | 确定 impl 返回哪个 effect 的本次操作表示 |
| 已准备的 require 参数 | 把业务值交给所选操作 |
| 受控 scope 使用入口 | 取得本次允许使用的资源能力 |
| 本次有效实现绑定 | 固定场景选择，支持追踪和 mock |

impl 的业务输入是 record 指定的 input effect；输出保持当前目标的注册与执行身份。loadImpl 的连接是 fileSession → loadText，reportImpl 的连接是 loadText → reportLoadError；这些是注册关系，不要求提升成不同的 Haskell 参数类型。

该接口构造待执行操作。后端驱动宿主 IO，再记录状态与业务结果。不能把“返回目标表示”实现为反复重入同一个 handler，也不能仅构造成功状态就当作 IO 已完成。

## 4. 模式匹配、存在封装与内部类型机制

模式匹配位于等号左侧，用来解构公开 ADT 或打开封装；句柄算子通常在等号右侧使用。必要时可在函数体内继续 case 匹配。

存在封装隐藏插件载体和内部类型，并携带可调用的能力字典。打开封装后，只能使用保留下来的操作和类型关系；封装本身不会给注册值生成新的名义类型。

associated data family / GADT 可以表达分类专用内容；associated type family 可以关联参数、结果和能力表示。它们留在框架与插件实现内，不是业务注册的必写语法，也不承担动态创建 instance 的工作。

统一描述和普通 Binding 不能复制活资源的线性所有权。拥有者凭证通过专用资源接口传递，具体规则见 [RESOURCE.md](RESOURCE.md)。

## 5. 绑定与错误处理的边界

一个 effect 可以提供多组句柄操作；具体多 impl 绑定表的结构尚待确定。当前基线是 handler 直接绑定函数，不能退回只用字符串列表猜测调用目标。

X 的 impl 可以记录、恢复或转换错误，但“日志输出成功”不自动意味着原业务恢复成功。恢复、重试和最终状态由显式错误协议决定。
