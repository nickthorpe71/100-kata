# 88 Regular Expression Matcher

## Difficulty
Hard

## Format
Single hard dynamic-programming problem.

## Time Target
45-60 minutes.

## Prompt
Implement regex matching with support for:
- `.` matching any single character
- `*` matching zero or more of the previous element

Return whether the full string matches the full pattern.

## Function Signature

```cpp
bool isRegexMatch(const string& s, const string& pattern);
```

## Interview Focus
- defining DP or recursion state precisely
- handling `*` cases without hand-waving
- testing tricky empty-string combinations
