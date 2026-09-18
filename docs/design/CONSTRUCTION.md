# Effect 构造原理：GADT、effect 与合并入口

本文件定义统一注册入口的内部构造。字段语义见 [EFFECT.md](EFFECT.md)，类方法与依赖注入见 [IMPL.md](IMPL.md)，直接使用方式见 [QUICKSTART.md](QUICKSTART.md)。

对外统一使用 `Effect { ... }`。内部以 GADT 保存具体类型关系，以 effect 函数添加 Free 层，再通过记录式模式同义词将两步合为一个入口。

## 1. 各个名字的职责

| 名字 | 职责 |
|---|---|
| EffectF next | 单层操作描述的数据类型，采用 GADT 写法 |
| CoreEffect | 内部值构造器，将字段和存在类型关系封装为一层描述 |
| effect | 将一层描述包装为完整 EffectChain 的普通函数 |
| Effect | 对外的记录式模式同义词，提供统一的构造和匹配入口 |
| EffectChain | Free 承载的完整递归描述类型 |

Effect 能力类型类位于类型命名空间；公开构造入口 Effect 位于值构造器命名空间。类方法与实例的定义见 [IMPL.md](IMPL.md)。

## 2. 数据定义：采用 GADT 写法

下面给出构造核心的最小定义，保留 typing、input、handler、bind 四个字段。EffectTyping 表示操作分类，Handlers e 表示类方法字典，Impl e 沿用 IMPL 文档中的实现接口约定。

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitForAll #-}

import Control.Monad.Free (Free(Free))

data EffectF next where
  CoreEffect ::
    EffectTyping ->  -- typing：操作分类
    next         ->  -- input：上游描述
    Handlers e   ->  -- handler：具体 effect 的类方法字典
    Impl e       ->  -- bind：绑定的实现函数
    EffectF next
```

这是一个采用 GADT 语法定义的代数数据类型。构造器内部的 e 关联 Handlers e 与 Impl e，结果类型 EffectF next 将 e 封装为存在类型。

Handlers e 携带对应分类的类方法字典，Impl e 保留实现与该具体 effect 的对应关系。解释器通过模式匹配打开封装，在恢复的类型关系下调用实现。存在封装机制参考 [GHC 文档](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/existential_quantification.html)。

完整注册继续包含 [EFFECT.md](EFFECT.md) 列出的 require、once、scope、layer、result 及分类专用字段。实现这些字段时，将它们加入内部描述和公开 record 的对应位置；X 的 help 由分类专用结构承接。这里的四字段代码用于呈现和验证构造核心，完整表面 API 按上述字段合同继续实现。

## 3. Free 与 effect 函数

input 是递归位置，Functor 只映射该位置：

```haskell
instance Functor EffectF where
  fmap f (CoreEffect kind upstream methods impl) =
    CoreEffect kind (f upstream) methods impl

type EffectChain = Free EffectF ()

effect :: EffectF EffectChain -> EffectChain
effect description =
  Free description

end :: EffectChain
end = pure ()
```

effect 的定义也可以写成 effect = Free。自然语言流程是：

1. CoreEffect 将分类、上游描述、类方法字典与 impl 封装为一层。
2. effect 为这一层添加 Free 构造。
3. 得到的 EffectChain 可作为另一份注册的 input。

内部展开形式例如：

```haskell
effect
  (CoreEffect Excu fileSession
    (handlers @LoadText) loadImpl)
```

Handlers e 和 Impl e 保持固定的调用类型，fmap 负责变换上游描述。Free 的 () 表示描述端点，业务结果由运行时保存。Free 接口参考 [Control.Monad.Free](https://hackage-content.haskell.org/package/free-5.2/docs/src/Control.Monad.Free.html)。

## 4. 将构造与包装合并为 Effect

框架定义一个双向的记录式模式同义词：

```haskell
pattern Effect :: () => forall e.
  EffectTyping -> EffectChain -> Handlers e -> Impl e -> EffectChain

pattern Effect { typing, input, handler, bind }
  <- Free (CoreEffect typing input handler bind)
  where
    Effect kind upstream methods impl =
      effect (CoreEffect kind upstream methods impl)
```

签名将公开结果固定为 EffectChain，并保留 e 的存在封装。这个定义同时表达两个方向：

- **构造**：用户填写 record，框架调用 CoreEffect 和 effect，得到 EffectChain。
- **匹配**：解释器匹配 Effect，展开 Free 与 CoreEffect，取得字段以及关联的存在类型关系。

因此四字段核心的公开写法是：

```haskell
loadText :: EffectChain
loadText = Effect
  { typing  = Excu
  , input   = fileSession
  , handler = handlers @LoadText
  , bind    = loadImpl
  }
```

该表达式与上一节的内部展开形式对应。完整注册示例见 [QUICKSTART.md](QUICKSTART.md)。

PatternSynonyms 扩展用于定义这个入口，业务模块按公开接口使用普通 record 构造写法；示例中的 @LoadText 使用 TypeApplications。记录式模式同义词支持构造和匹配，见 [GHC 文档](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html#record-pattern-synonyms)。

## 5. 构造与运行的分工

CoreEffect 与 effect 完成操作描述的构造。解释器在参数、控制条件与资源就绪后运行 bind 指定的 impl；impl 通过 handler 类方法获得依赖。

- input 承载句柄组合，Read/Map/require 承载业务值准备。
- record 的 bind 字段指定 impl，Free 的 >>= 负责端点替换与描述组合。
- scope、layer、help 保存可解析关联，资源所有权由专用运行接口管理。
- 执行身份、共享调用、监听轮次和模块工具由运行时维护。

由此，使用文档只需要说明如何填写 Effect record 和调用句柄，构造原理集中在本文件。
