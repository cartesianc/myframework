# AST：模块控制流与业务值

节点引用 [EFFECT.md](EFFECT.md) 中的注册；参数准备完成后交给 bind 指定的 [impl](IMPL.md)，供其通过 handler 句柄使用。AST 之外的事件入口见 [HANGING.md](HANGING.md)，运行规则见 [INTERPRETER.md](INTERPRETER.md)。

## 1. 用分支划分模块

AST 是递归控制树。命名分支形成模块边界，节点挂载 effect 及其参数准备表达式。

```haskell
data WorkflowF next
  = Node PreparedNode
  | Chained [next]
  | Parallel [next]
  | Choose Condition next next
  | Loop Watch next

type Workflow = Fix WorkflowF
```

以上只展示结构，模块名称等元数据省略。

| 分支 | 语义 |
|---|---|
| chained | 按声明顺序执行模块 |
| parallel | 调度可并行的模块 |
| either / Choose | 根据条件只执行选中的分支 |
| loop | 监听事件或变化，建立新的执行轮次 |

AST 是控制树，模块父子关系表达控制归属；input、值和 scope 引用分别形成操作组合、业务值和资源关系。

## 2. 在节点上显式准备值

节点的标准形态是“目标 effect + Read/Map 参数表达式”：

```haskell
node loadText $ mapped buildReadRange $ readFrom readRequest
```

readRequest 和 buildReadRange 分别引用已注册的 Read、Map effect。组合子都构造描述；Read 获取源执行的结果，Map 转换或汇合业务值，最终满足 loadText 的 require。

多个来源可以组合成一个参数 ADT，例如起始位置和长度汇成 ReadRange。input 仍只表达上游句柄组合。无参数节点归一化为空参数表达式，便捷写法待定。

result 字段描述结果契约，Read 读取对应执行实际产生的结果；pending 表示等待，failed 携带失败信息，succeeded 表示成功完成。

## 3. 惰性与等待分别实现

Haskell 惰性求值允许先构造、按需解释表达式。原生 $ 表达右结合的函数应用；解释器负责异步 IO、等待和唤醒。

节点只有在参数、控制条件和所需能力满足时才启动。等待期间后端保留等待原因，并在来源完成后恢复。

语言背景见 [Haskell Report：非严格求值](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x8-580006.2)。

## 4. 递归方案与控制执行

cata 将递归结构折叠为可调度计划；ana 可用于从种子展开结构。二者描述结构变换，实际事件循环和资源生命周期由运行时管理。

fold 保留延迟执行边界：解释器只运行选中的分支，并在事件到达时展开 loop 的下一轮。按执行身份共享同次上游调用，新监听轮次则创建新身份。

parallel 的失败传播与取消策略、loop 的变化判定与重入策略，以及流式参数的就绪粒度仍待确定。实现时先明确这些策略，再固定表面组合子。
