# Effect：注册与操作 IR

本文件定义公共描述。绑定函数见 [IMPL.md](IMPL.md)，资源字段见 [RESOURCE.md](RESOURCE.md)，运行解释见 [INTERPRETER.md](INTERPRETER.md)。

## 1. 一个统一 record

每个注册都是一个具体业务操作，但共用 EffectChain。名称、分类、配置和绑定区分注册，不为每个注册建立独立递归 newtype。

| 字段 | 含义 |
|---|---|
| 注册身份 | 注册集合提供稳定键；源码变量名便于阅读，不能自动成为运行时身份 |
| typing | Excu、Pure、Read、X、Map、Close |
| input | 唯一上游操作描述；根操作使用 end |
| require | 所需业务参数的契约；无参数时为空参数契约 |
| handler | 注册时显式绑定的 impl 函数 |
| result | 业务结果的契约 |
| once | 对操作或资源责任的线性要求 |
| scope | 资源拥有者关联的 Close effect；默认无 |
| layer | 对 scope 能力的要求，关联实际 effect 引用 |
| help | X 专用的错误处理目标；当前基线是资源拥有者的 scope |

require/result 是描述业务 ADT 的契约值。状态、已经产生的结果和活资源保存在运行时，不写回共享的静态 record。一次调用的 impl 外层返回 effect 表示，业务结果再由 Read 提供给消费者。

分类决定专用字段和句柄能力：

| typing | 行为 |
|---|---|
| Excu / e | 外部操作 |
| Pure / p | 内部纯计算 |
| Read / r | 读取某次 effect 执行的结果 |
| X / x | 处理指定范围的错误 |
| Map / m | 转换、组合业务值，为 require 准备参数 |
| Close / c | 结束 scope，释放其资源 |

这些是框架的操作分类。Pure 分类必须由实际纯接口支持，单靠标签不能保证纯度。分类专用内容可由内部 ADT、GADT 或 associated data family 组织；前台保持统一注册写法，不暴露内部类型机制。

## 2. input 是句柄组合

单一 input 表达由底层向上层组成操作的描述链，例如 C (B A)。它不承担业务值传递；多个值来源通过 AST 的 Read/Map 汇入 require。

上游可以已经是一个复合操作。业务配置或绑定发生变化时，增加或调整注册值即可，避免为 A1、A2 等配置变体创建大量类型。

handler 在 record 中直接绑定函数。连接关系由显式绑定确定，不额外要求为每条连接编写类型索引或身份证明。普通 Haskell 函数类型与类型类约束仍然适用。

## 3. Free 统一递归表示

以下是框架内部的最小结构，只展示递归位置与绑定：

```haskell
data CoreEffectF next = Effect
  { input   :: next
  , handler :: Impl
  }

instance Functor CoreEffectF where
  fmap f (Effect upstream impl) =
    Effect (f upstream) impl

type EffectChain = Free CoreEffectF ()

effect :: CoreEffectF EffectChain -> EffectChain
effect = Free

end :: EffectChain
end = pure ()
```

实际单层 record 还包含前述分类、契约、身份和资源字段。业务通过 effect 包装已经填好 input 的一层描述；这里使用 Free 构造器，与 liftF 自动添加端点的用途不同。

实现必须保留以下语义：

- input 是递归位置，完整 EffectChain 闭合递归；不能仅凭“有递归”就推出任意结构都是 Free。
- handler 使用固定的 Impl 类型。若写成 next → self，next 同时出现在函数入参位置，会破坏上述 Functor 映射。
- () 是描述端点；业务结果不放在这个端点。Free 的 Pure 构造器也不同于业务 Pure 分类。
- Free 的 bind 替换端点，用于组合描述；业务值传递仍走 Read/Map/require。
- input 朝向上游。直接在普通 foldFree 中先执行当前层，不能自动得到上游优先语义；应折叠出执行计划，再按需要驱动。
- scope、layer、help 等关联使用可解析的引用，避免把资源拥有者和关闭者的互相引用展开成无限树。
- Free 描述可以共享；执行身份、共享调用、资源所有权和控制循环由后端管理。

Free 减少手写递归包装，不自动提供调度、线性资源、序列化函数或 VM 工具能力。接口依据见 [Control.Monad.Free](https://hackage-content.haskell.org/package/free-5.2/docs/src/Control.Monad.Free.html)。

## 4. 三种身份分别保存

| 身份 | 用途 |
|---|---|
| 注册身份 | 标识静态操作及显式绑定 |
| 执行身份 | 区分同一注册的不同调用、监听轮次、压测请求 |
| scope 身份 | 标识本次资源拥有者及其有效范围 |

AST 模块路径补充执行位置。引用、诊断和结果读取都使用足够的身份信息，不能仅凭 Haskell 类型名或共享描述的地址推断一次执行。

## 5. 保留的设计方向

同一 IR 同时服务代码阅读、架构文档、配置启动和模块定位。原子句柄可以组合为更大的操作，而 scope 限制可使用的能力范围。类型层面的封装集中在框架和插件内部，业务主要填写声明、绑定实现和组合模块。
