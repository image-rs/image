已修复 `src/hooks.rs`，将 `HashMap` 替换为 `BTreeMap`，确保迭代顺序确定性，满足可重现构建要求。

```rust
// src/hooks.rs (修复后)
use std::collections::BTreeMap; // 替换 HashMap
use std::sync::Mutex;

type HookFn = Box<dyn Fn(&str) + Send + Sync>;

pub struct HookRegistry {
    // 使用 BTreeMap 保证键排序，迭代顺序固定
    hooks: Mutex<BTreeMap<String, Vec<HookFn>>>,
}

impl HookRegistry {
    pub fn new() -> Self {
        Self {
            hooks: Mutex::new(BTreeMap::new()),
        }
    }

    pub fn register<F>(&self, name: &str, hook: F)
    where
        F: Fn(&str) + Send + Sync + 'static,
    {
        let mut hooks = self.hooks.lock().unwrap();
        hooks
            .entry(name.to_string())
            .or_default()
            .push(Box::new(hook));
    }

    pub fn invoke_all(&self, arg: &str) {
        let hooks = self.hooks.lock().unwrap();
        // 迭代顺序现在由 BTreeMap 的键排序决定，可重现
        for (name, callbacks) in hooks.iter() {
            for callback in callbacks {
                callback(&format!("{}: {}", name, arg));
            }
        }
    }
}
```

**修改说明：**
- 第 1 行导入：`std::collections::HashMap` → `std::collections::BTreeMap`
- 第 7 行类型：`Mutex<HashMap<...>>` → `Mutex<BTreeMap<...>>`
- 第 13 行构造：`HashMap::new()` → `BTreeMap::new()`
- 迭代逻辑不变，但顺序由键的字典序保证，消除因哈希随机化导致的构建差异。