# 简版：有什么功能，怎么用

**请优先阅读文档理解最新设计。** 文档描述目标架构，已发布代码保留早期试验实现。以下是目标 API 的伪代码，旧版运行命令见 [IMPLEMENTATION.md](IMPLEMENTATION.md)。

## 有什么功能

| 功能 | 使用方式 |
|---|---|
| 注册业务操作 | 填写 Effect record，通过 bind 绑定 impl |
| 提供句柄 | 类型类声明 handler 方法，instance 为特定 effect 提供解释 |
| 使用依赖 | 在 impl 中调用句柄，使用已声明的参数和资源 |
| 组合操作 | input 引用上游 effect，组合已有句柄 |
| 准备参数 | require 声明参数，Read 取值，Map 转换 |
| 组织模块 | AST 声明串行、并行、分支和监听 |
| 管理资源和错误 | layer 要求资源，scope 管关闭，once 要求线性释放，X 处理指定范围的错误 |
| 观察和测试 | 按模块监听、mock、stress、log/show |
| 配置启动 | 保存声明为 JSON，加载时装配已编译的 effect 实例与 impl |

## 怎么用

以读取文件文本为例。插件提供文件 scope fileSession 和 effect 类型 LoadText；业务按下面的步骤注册、提供 instance 并编写 impl。

### 1. 直接填写 Effect record

```haskell
loadText :: EffectChain
loadText = Effect
  { typing  = Excu
  , input   = fileSession
  , require = readRangeContract
  , handler = handlers @LoadText
  , bind    = loadImpl
  , once    = False
  , scope   = noScope
  , layer   = need fileSession
  , result  = textResult
  }
```

- input 填上游 effect，layer 填所需文件 scope。
- require 和 result 填参数与结果契约。
- handler 通过 handlers @LoadText 选择句柄集合，bind 填绑定的 loadImpl。
- once = False 采用常规使用约束，scope = noScope 将文件生命周期交由 fileSession 管理。

### 2. 为具体 effect 提供类方法解释

```haskell
instance Effect 'Excu LoadText

instance Execution LoadText where
  execute upstream = do
    LoadText context <- currentEffect
    usingFile context upstream readTextIO
```

在 execute 中填写 LoadText 的读取行为。currentEffect 取得本次操作，usingFile 使用声明的文件能力调用 readTextIO。这些接口名称为目标 API 示意。

### 3. 编写已绑定的 impl

```haskell
loadImpl :: Impl LoadText
loadImpl upstream =
  execute upstream
```

loadImpl 接收 input 指定的上游 effect，调用 execute 使用参数和文件能力，完成本次 LoadText 操作。将其填入注册的 bind 字段。

### 4. 准备值并组织 AST

readRequest、buildReadRange 分别引用已注册的 Read、Map effect：

```haskell
node loadText $ mapped buildReadRange $ readFrom readRequest
```

用 chained、parallel、either、loop 组织节点。参数与资源就绪后，解释器运行绑定的 impl。回调、等待和事件恢复通过 Hanging 接入。

### 5. 启动并使用模块后端

将注册集合命名为 effect，控制树命名为 ast：

```haskell
main = interpreter ast effect
```

- **资源关闭**：scope 退出时运行 Close effect 绑定的 impl，关闭句柄消费释放责任。
- **监听**：选择模块，订阅开始、等待、完成、失败和关闭事件。
- **mock**：为运行场景选择句柄解释和 impl 绑定。
- **stress**：设置模块、输入、重复次数和并发度。
- **log/show**：查看连接、状态、等待原因及本次采用的句柄解释和 impl。
- **JSON 启动**：保存声明与插件标识，加载时恢复实例和绑定。

完整文档见[目录与文件关系](README.md)。
