# myframework

**请优先阅读文档理解最新设计。** `docs/design/` 描述目标架构和计划用法，已发布代码保留早期试验实现。两者处于各自的演进阶段，实际可运行能力以对应版本的源码为准。

以可读 Effect IR、Haskell EDSL 和递归 AST 为基础的声明式函数式架构。

handler 是类型类为具体 effect 提供的句柄方法，instance 给出 ad-hoc 解释。注册通过 bind 绑定 impl，impl 调用句柄时获得依赖注入；AST 组织模块控制流，interpreter 统一执行。模块后端面向监听、mock、stress 和 log/show。

```haskell
main = interpreter ast effect
```

## 文档

- [简版：有什么功能，怎么用](docs/design/QUICKSTART.md)
- [完整设计：目录与文件关系](docs/design/README.md)
- [代码差异、运行命令与实现计划](docs/design/IMPLEMENTATION.md)

最新设计统一放在 `docs/design/`。文中的 API 是目标写法；当前源码仍是早期试验实现。

现有 core 与发布证据的维护说明见 [trustbase/README.md](trustbase/README.md)。
