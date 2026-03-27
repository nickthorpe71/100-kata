# 78 Three Linked List Problems

## Format
Three linked-list questions in one round.

## Time Target
45-60 minutes total.

## Problem A
Reverse a singly linked list.

```cpp
ListNode* reverseList(ListNode* head);
```

## Problem B
Return `true` if a linked list has a cycle.

```cpp
bool hasCycle(ListNode* head);
```

## Problem C
Reorder a linked list from `L0 -> L1 -> ... -> Ln` to `L0 -> Ln -> L1 -> Ln-1 -> ...`.

```cpp
void reorderList(ListNode* head);
```

## Interview Focus
- pointer discipline
- not losing references mid-edit
- explaining why each phase of a list algorithm is safe
