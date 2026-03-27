# 96 Three Recursion Backtracking Problems

## Format
Three recursion-focused problems in one round.

## Time Target
45-60 minutes total.

## Problem A
Return all subsets of a list of unique integers.

```cpp
vector<vector<int>> subsets(const vector<int>& nums);
```

## Problem B
Return all combinations where numbers may be reused and sum to `target`.

```cpp
vector<vector<int>> combinationSum(const vector<int>& candidates, int target);
```

## Problem C
Return all valid strings containing `n` pairs of parentheses.

```cpp
vector<string> generateParentheses(int n);
```

## Interview Focus
- forcing yourself to define recursive state before code
- backtracking with controlled mutation
- avoiding duplicate or invalid branches
