# Competitive Programming Cheat Sheet

**Quick Reference for Technical Interviews**

---

## Pattern Recognition Table

| If Problem Has... | Think... | Time | Key Technique |
|-------------------|----------|------|---------------|
| Subarray/substring with constraint | Sliding Window | O(n) | Two pointers: expand right, shrink left |
| Next greater/smaller | Monotonic Stack | O(n) | Stack tracks pending elements |
| K largest/smallest | Heap | O(n log k) | Min/max heap of size k |
| Two sum variants | HashMap | O(n) | Store complement |
| Intervals/meetings | Greedy + Sort | O(n log n) | Sort by start, merge overlaps |
| All combinations | Backtracking | O(2^n) | Try, recurse, undo |
| Optimal with overlapping | DP | Varies | Memo/tabulation |
| Shortest path (unweighted) | BFS | O(V+E) | Queue, level-by-level |
| Connected components | DFS/Union Find | O(V+E) | Mark visited or disjoint set |
| Task dependencies | Topological Sort | O(V+E) | Track in-degrees |
| Prefix matching | Trie | O(m) | Tree of characters |

---

## Code Templates

### Sliding Window (Variable Size)
```cpp
int maxLen = 0, left = 0;
unordered_map<char, int> count;
for (int right = 0; right < n; right++) {
  count[s[right]]++;
  while (invalid) { count[s[left++]]--; }
  maxLen = max(maxLen, right - left + 1);
}
```

### Binary Search
```cpp
int left = 0, right = n - 1;
while (left <= right) {
  int mid = left + (right - left) / 2;
  if (arr[mid] == target) return mid;
  else if (arr[mid] < target) left = mid + 1;
  else right = mid - 1;
}
```

### DFS (Grid)
```cpp
bool dfs(vector<vector<int>>& grid, int x, int y) {
  if (out of bounds || visited) return false;
  if (found target) return true;
  grid[x][y] = VISITED;
  return dfs(grid, x+1, y) || dfs(grid, x-1, y) ||
         dfs(grid, x, y+1) || dfs(grid, x, y-1);
}
```

### BFS (Level Order)
```cpp
queue<Node*> q;
q.push(root);
while (!q.empty()) {
  Node* node = q.front(); q.pop();
  process(node);
  if (node->left) q.push(node->left);
  if (node->right) q.push(node->right);
}
```

### Backtracking
```cpp
void backtrack(state, path, result) {
  if (is_solution(state)) { result.push_back(path); return; }
  for (choice in choices) {
    if (is_valid(choice)) {
      make_choice(choice);
      backtrack(new_state, path, result);
      undo_choice(choice);  // BACKTRACK
    }
  }
}
```

### Union Find
```cpp
class UnionFind {
  vector<int> parent, rank;
public:
  UnionFind(int n) : parent(n), rank(n, 0) {
    iota(parent.begin(), parent.end(), 0);
  }
  int find(int x) {
    if (parent[x] != x) parent[x] = find(parent[x]);
    return parent[x];
  }
  bool unite(int x, int y) {
    int rx = find(x), ry = find(y);
    if (rx == ry) return false;
    if (rank[rx] < rank[ry]) swap(rx, ry);
    parent[ry] = rx;
    if (rank[rx] == rank[ry]) rank[rx]++;
    return true;
  }
};
```

### Topological Sort (Kahn's)
```cpp
vector<int> topoSort(int n, vector<pair<int,int>>& edges) {
  vector<vector<int>> graph(n);
  vector<int> inDegree(n, 0);
  for (auto [u, v] : edges) {
    graph[u].push_back(v);
    inDegree[v]++;
  }
  queue<int> q;
  for (int i = 0; i < n; i++)
    if (inDegree[i] == 0) q.push(i);

  vector<int> result;
  while (!q.empty()) {
    int node = q.front(); q.pop();
    result.push_back(node);
    for (int nbr : graph[node])
      if (--inDegree[nbr] == 0) q.push(nbr);
  }
  return result.size() == n ? result : vector<int>{};
}
```

### Monotonic Stack (Next Greater)
```cpp
vector<int> nextGreater(vector<int>& nums) {
  int n = nums.size();
  vector<int> result(n, -1);
  stack<int> s;  // indices
  for (int i = 0; i < n; i++) {
    while (!s.empty() && nums[s.top()] < nums[i]) {
      result[s.top()] = nums[i];
      s.pop();
    }
    s.push(i);
  }
  return result;
}
```

---

## C++ STL Quick Picks

| Need | Use |
|------|-----|
| Random access | `vector<T>` |
| Unique elements, O(1) lookup | `unordered_set<T>` |
| Key-value, O(1) lookup | `unordered_map<K,V>` |
| Sorted unique elements | `set<T>` |
| Sorted key-value | `map<K,V>` |
| Min heap | `priority_queue<T, vector<T>, greater<T>>` |
| Max heap | `priority_queue<T>` |
| FIFO | `queue<T>` |
| LIFO | `stack<T>` |
| Both ends | `deque<T>` |

---

## Complexity Reference

| Operation | Best | Average | Worst |
|-----------|------|---------|-------|
| Array access | O(1) | O(1) | O(1) |
| Binary search | O(log n) | O(log n) | O(log n) |
| Hash lookup | O(1) | O(1) | O(n) |
| Tree lookup | O(log n) | O(log n) | O(n) |
| Heap insert | O(1) | O(log n) | O(log n) |
| Heap extract | O(log n) | O(log n) | O(log n) |
| Sort | O(n log n) | O(n log n) | O(n²) |
| DFS/BFS | O(V+E) | O(V+E) | O(V+E) |

---

## Edge Cases Checklist

- [ ] Empty input
- [ ] Single element
- [ ] All duplicates
- [ ] Negative numbers / zero
- [ ] Max/min integer values
- [ ] Sorted vs unsorted
- [ ] Odd vs even length

---

## Interview Framework

**1. CLARIFY (2 min)**
- Input constraints? (size, range)
- Empty/null possible?
- Duplicates allowed?
- Sorted?

**2. EXAMPLE (1 min)**
- Walk through normal case
- Edge case

**3. APPROACH (2 min)**
- "This is [PATTERN] because..."
- "Brute force: O(...)"
- "Optimized: O(...) using [TECHNIQUE]"

**4. CODE (10-15 min)**
- Meaningful names
- Handle edge cases
- Comment tricky parts

**5. TEST (2-3 min)**
- Dry run with example
- Edge cases

---

## Common Gotchas

```cpp
// WRONG: unsigned underflow
for (int i = 0; i < v.size() - 1; i++)

// RIGHT:
for (int i = 0; i + 1 < v.size(); i++)

// WRONG: modifies map when checking
if (map[key]) ...

// RIGHT:
if (map.count(key)) ...

// WRONG: overflow in mid
int mid = (left + right) / 2;

// RIGHT:
int mid = left + (right - left) / 2;
```

---

**Practice:** Master these 15 problems for 80% coverage:
Two Sum, Valid Parentheses, Reverse LL, Max Subarray, Climbing Stairs,
Binary Tree Level Order, Longest Substring, Number of Islands, Coin Change,
Merge Intervals, Top K Elements, Course Schedule, Word Search, LRU Cache, Binary Search
