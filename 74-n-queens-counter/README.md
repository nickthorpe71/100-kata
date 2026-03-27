# 74 N-Queens Counter

## Difficulty
Hard

## Prompt
Given an integer `n`, return the number of distinct ways to place `n` queens on an `n x n` chessboard so that no two queens attack each other.

A queen attacks:
- horizontally
- vertically
- diagonally

You must solve this using recursion and backtracking.

## Function Signature

```js
function countQueenArrangements(n) {}
```

## Example 1

```js
countQueenArrangements(1) // 1
```

## Example 2

```js
countQueenArrangements(4) // 2
```

## Example 3

```js
countQueenArrangements(5) // 10
```

## Constraints
- `1 <= n <= 9`

## Interview Focus
- recursive search by row
- backtracking
- efficient conflict checking
- choosing the right supporting data structures

## Follow-up
How would you track used columns and diagonals so each validity check is `O(1)`?
