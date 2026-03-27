# 73 Message Decode Count

## Difficulty
Medium-Hard

## Prompt
A message is encoded as a string of digits using the mapping:

- `1 -> A`
- `2 -> B`
- ...
- `26 -> Z`

Given a string `s` containing only digits, return the number of different ways to decode it.

A valid decoding must use:
- one digit from `1` to `9`, or
- two digits from `10` to `26`

You must solve this using recursion. A memoized solution is recommended.

## Function Signature

```js
function countDecodings(s) {}
```

## Example 1

```js
countDecodings("12") // 2
```

Explanation:
- `"1" + "2"` -> `"AB"`
- `"12"` -> `"L"`

## Example 2

```js
countDecodings("226") // 3
```

## Example 3

```js
countDecodings("06") // 0
```

## Constraints
- `1 <= s.length <= 100`
- `s` contains only digits

## Interview Focus
- recursion over string positions
- defining base cases clearly
- pruning invalid branches
- memoization to avoid repeated work

## Follow-up
Can you explain the difference between the plain recursive solution and the memoized version in time complexity?
