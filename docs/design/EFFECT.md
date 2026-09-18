# Effect：注册与操作 IR

本文件定义公共描述。handler、bind 和依赖注入见 [IMPL.md](IMPL.md)，资源字段见 [RESOURCE.md](RESOURCE.md)，运行解释见 [INTERPRETER.md](INTERPRETER.md)。

## 1. 一个统一 record

每个注册表达一个具体业务操作，公共类型统一为 EffectChain。名称、分类、配置、句柄解释和 impl 绑定共同确定其含义。具体 effect 的表示与能力字典保存在内部存在包中。

| 字段 | 含义 |
|---|---|
| 注册身份 | 注册集合提供的稳定键；源码变量名服务于阅读 |
| typing | Excu、Pure、Read、X、Map、Close |
| input | 唯一上游操作描述；根操作使用 end |
| require | 所需业务参数的契约；无参数时使用空参数契约 |
| handler | 类型类为具体 effect 提供的句柄方法集合，通过 instance 获得解释 |
| bind | 注册显式绑定的 impl；外部操作的 impl 是使用句柄的原子 IO 实现单位 |
| result | 业务结果的契约 |
| once | 对操作或资源责任的线性要求 |
| scope | 资源拥有者关联的 Close effect；默认无 |
| layer | 对 scope 能力的要求，关联实际 effect 引用 |
| help | X 专用的错误处理目标；当前基线是资源拥有者的 scope |

例如 handler = handlers @LoadText 选择 LoadText 的类方法集合，bind = loadImpl 绑定具体函数。构造 effect 时，框架封装具体表示、类型类字典及 impl，供解释器按本次执行使用。

require/result 是描述业务 ADT 的契约值。状态、业务结果和活资源保存在运行时。impl 以 input effect 为输入，并在调用环境中返回本次目标 effect；Read 将业务结果提供给消费者。

分类决定专用字段和句柄能力：

| typing | 行为 |
|---|---|
| Excu / e | 外部操作 |
| Pure / p | 内部纯计算 |
| Read / r | 读取某次 effect 执行的结果 |
| X / x | 处理指定范围的错误 |
| Map / m | 转换、组合业务值，为 require 准备参数 |
| Close / c | 结束 scope，释放其资源 |

Pure 的语义由纯计算接口落实。分类专用内容由框架内部的 ADT、GADT 或 associated data family 组织，业务通过统一 record 使用这些能力。

## 2. input 是句柄组合

单一 input 表达由底层向上层组成操作的描述链，例如 C (B A)。多个业务值来源通过 AST 的 Read/Map 汇入 require。

上游可以已经是一个复合操作。业务配置或绑定发生变化时，通过注册值表达变体，同一具体 effect 表示及其 instance 可以复用。

handler 的 instance 解释提供句柄能力，bind 确定执行哪个 impl。impl 使用类方法时，依赖随当前 effect 的能力字典和调用环境注入。连接关系由显式绑定确定，普通 Haskell 函数类型与类型类约束负责各自的接口关系。

## 3. Free 统一递归表示

以下是注册封装完成后的框架内部结构，只展示递归位置与操作包：

```haskell
data CoreEffectF next = EffectF
  { input     :: next
  , operation :: SomeOperation
  }

instance Functor CoreEffectF where
  fmap f (EffectF upstream operation) =
    EffectF (f upstream) operation

type EffectChain = Free CoreEffectF ()

end :: EffectChain
end = pure ()
```

SomeOperation 是内部存在包，保存具体 effect 表示、分类与契约、handler 能力字典及 bind 指定的 impl。前台 effect 构造函数将填好的 record 封装为一层操作包，再放入 Free 描述。

表示遵循以下规则：

- input 是递归位置，完整 EffectChain 闭合递归。
- operation 的封装类型固定，fmap 只映射 input，内部方法和 impl 保留各自的调用类型。
- () 表示描述端点；业务结果由运行时管理。Free 的 Pure 构造器表示端点，业务 Pure 分类表示纯计算。
- Free 的 bind，即 >>=，替换端点以组合描述；record 的 bind 字段指定 impl 函数。
- input 朝向上游。折叠先构造执行计划，再按分类和依赖状态驱动所需操作。
- scope、layer、help 等关联保存可解析引用；拥有者与关闭者通过引用相连。
- Free 描述可共享；执行身份、共享调用、资源所有权和控制循环由后端管理。

Free 提供统一递归与描述组合，执行器提供调度、资源和模块工具能力。接口依据见 [Control.Monad.Free](https://hackage-content.haskell.org/package/free-5.2/docs/src/Control.Monad.Free.html)。

## 4. 身份与存在封装

| 内容 | 用途 |
|---|---|
| 具体 effect 类型与字典 | 确定类方法的 ad-hoc 解释，由存在包保存 |
| 注册身份 | 标识静态操作、handler 解释和 impl 绑定 |
| 执行身份 | 标识具体调用、监听轮次或压测请求 |
| scope 身份 | 标识本次资源拥有者及其有效范围 |

AST 模块路径补充执行位置。引用、诊断、依赖注入和结果读取均携带对应身份；同一实例解释可以服务多份注册和多次执行。

## 5. 保留的设计方向

同一 IR 同时服务代码阅读、架构文档、配置启动和模块定位。原子句柄组合成更大的操作，scope 限定可使用的能力范围。内部类型机制承接分类、存在封装和字典传递，业务主要填写声明、提供 instance、绑定 impl 和组合模块。
