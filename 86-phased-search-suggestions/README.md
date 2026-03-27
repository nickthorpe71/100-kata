# 86 Phased Search Suggestions

## Format
Evolving product requirements.

## Time Target
45-60 minutes.

## Phase 1
Given a list of products and a search word, return up to `3` lexicographically smallest suggestions after each typed character.

## Phase 2
Support `addProduct(product)` so the catalog can change after initialization.

## Phase 3
Now each product has a popularity score. Suggest up to `3` products by:
1. higher popularity
2. lexicographically smaller name on ties

## Starting Skeleton

```cpp
class SearchSuggester {
public:
    explicit SearchSuggester(const vector<string>& products);
    void addProduct(const string& product);
    vector<vector<string>> suggest(const string& searchWord) const;
};
```

## Interview Focus
- sorting and prefix search tradeoffs
- adapting a clean initial design
- being explicit about what must change between phases
