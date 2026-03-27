# 85 LRU Cache

## Difficulty
Hard

## Format
Single hard problem with low interviewer guidance.

## Time Target
45-60 minutes.

## Prompt
Design an `LRUCache` that supports `get(key)` and `put(key, value)` in `O(1)` average time.

- `get(key)` returns the value if the key exists, otherwise `-1`
- `put(key, value)` inserts or updates the key
- when capacity is exceeded, evict the least recently used key

## Class Skeleton

```cpp
class LRUCache {
public:
    explicit LRUCache(int capacity);
    int get(int key);
    void put(int key, int value);
};
```

## Example
`LRUCache cache(2)` -> `put(1, 1)` -> `put(2, 2)` -> `get(1) == 1` -> `put(3, 3)` evicts key `2`.

## Interview Focus
- linked list plus hash map design
- keeping invariants stable during updates
- communicating why each operation is `O(1)`
