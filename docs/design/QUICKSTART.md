# 简版：有什么功能，怎么用

**请优先阅读文档理解最新设计。** 文档描述目标架构，已发布代码保留早期试验实现。以下是目标 API 的伪代码，旧版运行命令见 [IMPLEMENTATION.md](IMPLEMENTATION.md)。

## 有什么功能

| 功能 | 使用方式 |
|---|---|
| 注册业务操作 | 填写统一 effect record，通过 bind 绑定 impl |
| 提供句柄 | 类型类声明 handler 方法，instance 为特定 effect 提供解释 |
| 注入依赖 | impl 调用句柄，框架使用该 effect 携带的实例解释和调用环境 |
| 组合操作 | input 引用上游 effect，组合已有句柄 |
| 准备参数 | require 声明参数，Read 取值，Map 转换 |
| 组织模块 | AST 声明串行、并行、分支和监听 |
| 管理资源和错误 | layer 要求资源，scope 管关闭，once 要求线性释放，X 处理指定范围的错误 |
| 观察和测试 | 按模块监听、mock、stress、log/show |
| 配置启动 | 保存声明为 JSON，加载时装配已编译的 effect 实例与 impl |

## 怎么用

以读取文件文本为例。插件提供文件 scope 和具体 effect 的内部表示 LoadText；业务填写注册并绑定原子 IO 实现。

### 1. 注册 effect，绑定 impl

```haskell
loadText :: EffectChain
loadText = effect $ Effect
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

handler 是 LoadText 的类型类句柄集合；handlers @LoadText 取得对应 instance 的类方法。bind 是原子 IO 实现 loadImpl 的绑定位置。effect 将具体表示、句柄字典和绑定打包为统一描述。

### 2. 为具体 effect 提供类方法解释

```haskell
instance Effect 'Excu LoadText

instance Execution LoadText where
  execute upstream = do
    LoadText context <- currentEffect
    usingFile context upstream readTextIO
```

execute 是 handler 句柄。这个 instance 为 LoadText 提供 ad-hoc 多态解释：从本次调用取得参数和文件能力，完成对应的读取操作。usingFile 和 currentEffect 是框架接口的示意名称。

### 3. impl 使用句柄，依赖随实例注入

```haskell
loadImpl :: Impl LoadText
loadImpl upstream =
  execute upstream
```

loadImpl 是注册绑定的原子 IO 实现单位。upstream 是 input 指定的 effect；execute 使用 LoadText 的实例解释，返回本次目标 effect。框架负责传递实例字典、参数和 scope 使用入口。

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

原理见 [IMPL.md](IMPL.md)，完整设计见[文件关系](README.md)。
