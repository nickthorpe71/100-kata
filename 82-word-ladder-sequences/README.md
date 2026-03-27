# 82 Word Ladder Sequences

## Difficulty
Hard

## Format
Single hard problem. Expect follow-up questions after correctness.

## Time Target
45-60 minutes.

## Prompt
Given `beginWord`, `endWord`, and a dictionary, return all shortest transformation sequences from `beginWord` to `endWord`.

Rules:
- only one letter may change at a time
- every intermediate word must exist in the dictionary
- if no sequence exists, return an empty result

## Function Signature

```cpp
vector<vector<string>> findLadders(
    const string& beginWord,
    const string& endWord,
    const vector<string>& wordList
);
```

## Interview Focus
- BFS to discover shortest depth
- DFS or backtracking to rebuild sequences
- not exploding runtime unnecessarily
