# 77 Phased Vote Counter

## Format
Evolving requirements. Do not rewrite recklessly between phases.

## Time Target
45-60 minutes.

## Phase 1
Implement a `VoteCounter` that supports `addVote(candidate)` and `topCandidate()`.
If there is a tie, return the lexicographically smaller candidate name.

## Phase 2
Product now needs `removeVote(candidate)` to handle corrected ballots.
Do not let counts go below zero.

## Phase 3
Add `topK(k)` returning the top `k` candidates ordered by:
1. higher vote count
2. lexicographically smaller name on ties

## Starting Skeleton

```cpp
class VoteCounter {
public:
    void addVote(const string& candidate);
    void removeVote(const string& candidate);
    string topCandidate() const;
    vector<string> topK(int k) const;
};
```

## Interview Focus
- preserving a partially working solution
- handling ties carefully
- discussing tradeoffs between simple recomputation and faster queries
