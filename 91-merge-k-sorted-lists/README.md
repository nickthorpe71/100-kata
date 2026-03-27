# 91 Merge K Sorted Lists

## Difficulty
Hard

## Format
Single hard problem with implementation pressure.

## Time Target
45-60 minutes.

## Prompt
You are given `k` sorted linked lists. Merge them into one sorted linked list and return the head.

## Function Signature

```cpp
ListNode* mergeKLists(vector<ListNode*>& lists);
```

## Follow-up
Compare these approaches:
- repeatedly merging two lists
- using a min-heap
- divide and conquer

## Interview Focus
- choosing a scalable merge strategy
- linked-list pointer safety
- complexity analysis with `k` lists and `n` total nodes
