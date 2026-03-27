# 100 Autocomplete System

## Difficulty
Hard

## Format
Single hard design problem with product-style follow-ups.

## Time Target
45-60 minutes.

## Prompt
Design an `AutocompleteSystem` initialized with historical sentences and frequencies.

Support:
- `input(c)` where `c` is a character
- if `c != '#'`, return the top `3` matching sentences for the current prefix ordered by:
1. higher frequency
2. lexicographically smaller sentence on ties
- if `c == '#'`, commit the current sentence into the system and return an empty list

## Starting Skeleton

```cpp
class AutocompleteSystem {
public:
    AutocompleteSystem(const vector<string>& sentences, const vector<int>& times);
    vector<string> input(char c);
};
```

## Interview Focus
- trie or map-based design tradeoffs
- ranking and tie-breaking
- describing how you would scale writes and reads if the data set grows
