# myframework

**文档与当前发布代码不一致，请优先阅读文档。** 本次只更新设计文档；已发布代码仍是旧试验版，尚未实现最新设计。理解架构和计划用法，以 `docs/design/` 为准；实际可运行能力以对应版本的源码为准。

以可读 Effect IR、Haskell EDSL 和递归 AST 为基础的声明式函数式架构。

注册 effect 时绑定 impl，类型类方法提供句柄，AST 组织模块控制流，interpreter 统一执行。模块后端面向监听、mock、stress 和 log/show。

```haskell
main = interpreter ast effect
```

## 文档

- [简版：有什么功能，怎么用](docs/design/QUICKSTART.md)
- [完整设计：目录与文件关系](docs/design/README.md)
- [代码差异、运行命令与实现计划](docs/design/IMPLEMENTATION.md)

最新设计统一放在 `docs/design/`。文中的 API 是目标写法；当前源码仍是早期试验实现。

现有 core 与发布证据的维护说明见 [trustbase/README.md](trustbase/README.md)。
