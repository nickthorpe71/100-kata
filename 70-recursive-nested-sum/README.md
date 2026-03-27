# 70 Recursive Nested Sum

## Difficulty
Easy

## Prompt
Given a nested array of integers, return the sum of every integer in the structure.

The array can contain:
- integers
- empty arrays
- arrays that contain more integers or arrays

You must solve this using recursion.

## Function Signature

```js
function nestedSum(items) {}
```

## Example 1

```js
nestedSum([1, [2, 3], 4]) // 10
```

## Example 2

```js
nestedSum([[1, [2]], [], 3, [4, [5, [6]]]]) // 21
```

## Constraints
- `0 <= items.length <= 10^4`
- Each integer is in the range `-10^6` to `10^6`
- Maximum nesting depth is `1000`

## Interview Focus
- recursion base cases
- recursive traversal of nested data
- keeping state local to each call

## Follow-up
Can you also solve it iteratively with an explicit stack?
