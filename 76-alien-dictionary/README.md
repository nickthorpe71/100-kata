# 76 Alien Dictionary

## Difficulty
Hard

## Format
Single hard graph problem with ambiguity handling.

## Time Target
45-60 minutes.

## Prompt
You are given words sorted according to the rules of an unknown alien language. Return one valid character ordering. If no valid ordering exists, return an empty string.

## Function Signature

```cpp
string alienOrder(const vector<string>& words);
```

## Edge Cases
- prefix invalidity such as `["abc", "ab"]`
- disconnected characters
- cycles in the precedence graph

## Interview Focus
- graph construction from adjacent words
- topological sort
- explicit handling of invalid input
