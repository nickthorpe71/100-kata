# 72 Word Search Backtracking

## Difficulty
Medium-Hard

## Prompt
Given a 2D grid of characters and a word, return `true` if the word exists in the grid and `false` otherwise.

The word can be built from letters of sequentially adjacent cells, where adjacent means:
- up
- down
- left
- right

The same cell cannot be used more than once in a single path.

You must solve this using recursion and backtracking.

## Function Signature

```js
function exists(board, word) {}
```

## Example 1

```js
exists(
  [
    ["A", "B", "C", "E"],
    ["S", "F", "C", "S"],
    ["A", "D", "E", "E"],
  ],
  "ABCCED"
) // true
```

## Example 2

```js
exists(
  [
    ["A", "B", "C", "E"],
    ["S", "F", "C", "S"],
    ["A", "D", "E", "E"],
  ],
  "ABCB"
) // false
```

## Constraints
- `1 <= rows, cols <= 6`
- `1 <= word.length <= 15`
- All characters are uppercase English letters

## Interview Focus
- recursive DFS
- backtracking
- marking and unmarking state safely
- pruning invalid paths early

## Follow-up
What is the time complexity, and what pruning opportunities can reduce real-world runtime?
