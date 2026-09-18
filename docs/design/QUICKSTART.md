# 简版：有什么功能，怎么用

**文档与当前发布代码不一致，请优先按本文理解最新用法。** 以下是目标 API 的伪代码，尚不能直接用于旧版代码。已发布试验版的运行命令见 [IMPLEMENTATION.md](IMPLEMENTATION.md)。

## 有什么功能

| 功能 | 使用方式 |
|---|---|
| 注册业务操作 | 填写统一 effect record，直接绑定 impl |
| 组合操作 | input 引用上游 effect，组合已有句柄 |
| 准备业务参数 | require 声明参数，Read 取值，Map 转换 |
| 替换具体实现 | 类型类 instance 提供句柄解释，impl 选择使用 |
| 组织模块 | AST 声明串行、并行、分支和监听 |
| 管理资源和错误 | layer 要求资源，scope 管关闭，once 要求线性释放，X 处理指定范围的错误 |
| 接入回调和等待 | 注册 Hanging，由后端恢复执行 |
| 观察和测试模块 | 按模块监听、mock、stress、log/show |
| 配置启动 | 保存声明为 JSON，加载时绑定已编译插件 |

## 怎么用

以“在文件 scope 中读取文本”为例。假设插件已提供 fileSession、FileBackend 及读文件原子操作；业务提供读取范围和文本结果的契约值。

### 1. 注册 effect，当场绑定 impl

```haskell
loadText :: EffectChain
loadText = effect $ Effect
  { typing  = Excu
  , input   = fileSession
  , require = readRangeContract
  , handler = loadImpl
  , once    = False
  , scope   = noScope
  , layer   = need fileSession
  , result  = textResult
  }
```

input 组合文件操作，layer 要求文件 scope。handler 现在就绑定函数，函数体写在后面。

### 2. 提供类型类句柄的解释

框架声明能力类；插件通过 instance 实现。如果已有适合的插件，直接使用它。

```haskell
instance Execution FileBackend where
  execute backend binding upstream =
    external backend binding upstream readTextIO
```

execute 是句柄算子；readTextIO 是宿主原子操作。这里的组合子构造待执行操作。

### 3. 编写已经绑定的 impl

```haskell
loadImpl :: Impl
loadImpl binding upstream =
  execute fileBackend binding upstream
```

binding 由框架提供，携带本次目标和已准备参数。upstream 是注册里的 input；返回的是本次 loadText 的操作表示。

### 4. 在 AST 节点上准备值

假设 readRequest、buildReadRange 分别引用已注册的 Read、Map effect：

```haskell
node loadText $ mapped buildReadRange $ readFrom readRequest
```

用 chained、parallel、either、loop 组织这些节点成为 ast。参数和资源就绪后，解释器才执行对应操作。

### 5. 装配并启动

将注册放入 effect 集合，把控制树命名为 ast；入口模块写：

```haskell
main = interpreter ast effect
```

文件 scope 退出时，由它绑定的 Close effect 释放资源；业务读写节点通过 layer 使用该 scope。

### 6. 从模块后端观察和测试

- **监听**：选择模块，订阅开始、等待、完成、失败和关闭事件。
- **mock**：给一个运行场景配置替代 impl 绑定。
- **stress**：设置模块、输入、重复次数和并发度，发起独立调用。
- **log/show**：按模块查看声明关系、状态、等待原因、耗时和实际实现。
- **JSON 启动**：保存声明及实现标识，加载器从插件恢复绑定后启动。

完整规则按[目录中的文件关系](README.md)查阅。
