# 71 Tree Path Sum Count

## Difficulty
Medium

## Prompt
Given the root of a binary tree and a target sum, return the number of root-to-leaf paths whose values add up to the target.

A root-to-leaf path:
- starts at the root
- ends at a node with no children
- includes every node along that branch

You must solve this using recursion.

## Function Signature

```js
function countTargetPaths(root, targetSum) {}
```

## Example

```js
 const root = {
  value: 5,
  left: {
    value: 4,
    left: {
      value: 11,
      left: { value: 7, left: null, right: null },
      right: { value: 2, left: null, right: null },
    },
    right: null,
  },
  right: {
    value: 8,
    left: { value: 13, left: null, right: null },
    right: {
      value: 4,
      left: null,
      right: { value: 1, left: null, right: null },
    },
  },
};

countTargetPaths(root, 22) // 1
```

## Constraints
- Number of nodes is in the range `0` to `10^5`
- Node values are in the range `-10^4` to `10^4`

## Interview Focus
- recursive tree traversal
- leaf detection
- carrying partial state through recursive calls

## Follow-up
How would you adapt this to count any downward path, not just root-to-leaf paths?
