# Competitive Programming Quick Reference

**Goal:** Ace senior-level technical interviews and solve medium LeetCode problems in 20 minutes

---

## Table of Contents

### Part I: Core Patterns (1-10)
1. [Array/String Manipulation](#1-arraystring-manipulation)
2. [Trees & Graphs](#2-trees--graphs)
3. [Dynamic Programming](#3-dynamic-programming)
4. [Backtracking & Recursion](#4-backtracking--recursion)
5. [Greedy & Intervals](#5-greedy--intervals)
6. [Math & Number Theory](#6-math--number-theory)
7. [Bit Manipulation](#7-bit-manipulation)
8. [Stack/Queue](#8-stackqueue)
9. [Binary Search](#9-binary-search)
10. [Linked Lists](#10-linked-lists)

### Part II: Advanced Patterns (11-15)
11. [Sliding Window](#11-sliding-window)
12. [Monotonic Stack/Queue](#12-monotonic-stackqueue)
13. [Union Find (Disjoint Set)](#13-union-find-disjoint-set)
14. [Trie (Prefix Tree)](#14-trie-prefix-tree)
15. [Topological Sort](#15-topological-sort)

### Part III: Interview Strategy
- [Quick Decision Tree](#quick-decision-tree)
- [20-Minute Strategy](#20-minute-strategy)
- [Complexity Cheat Sheet](#complexity-cheat-sheet)
- [Common Edge Cases Checklist](#common-edge-cases-checklist)

### Part IV: Interview Excellence
- [Interview Communication Framework](#interview-communication-framework)
- [Pattern Recognition Shortcuts](#pattern-recognition-shortcuts)
- [C++ STL Quick Reference](#c-stl-quick-reference)
- [Debugging in Interviews](#debugging-in-interviews)
- [Practice Roadmap](#practice-roadmap)
- [System Design Context](#system-design-context)

---

## Part I: Core Patterns

## Pattern Recognition Map

### 1. ARRAY/STRING MANIPULATION

**Signals:**
- Simple transformations on collections (map, filter, reduce operations)
- Sorting can simplify the problem (pairing, finding duplicates, k-th element)
- Need to pair/combine elements optimally (min sum, max product)
- In-place modifications required (reverse, rotate, partition)
- Problems mention "rearrange", "reorder", "minimize/maximize sum"
- Finding k largest/smallest elements

**Patterns:**

**Sort + Two Pointers** (O(n log n))

*Use when you need to pair elements optimally. Sorting brings related elements together, then two pointers from opposite ends can find the best combinations. Common in problems asking to minimize/maximize sums or products of pairs.*

```cpp
int minSum(vector<int>& arr) {
  sort(arr.begin(), arr.end());
  int sum = 0;
  int n = arr.size();

  for (int i = 0; i < n / 2; i++) {
    sum += arr[i] * arr[n - 1 - i];
  }
  return sum;
}
```

**Top-K Selection** (O(n log n))

*Use when you need the k largest/smallest elements. Sort in the desired order and take the first k elements. For better performance, use a heap (O(n log k)) or quickselect (O(n) average).*

```cpp
long long maxProduct(vector<int>& nums, int k) {
  sort(nums.begin(), nums.end(), greater<int>());
  long long product = 1;

  for (int i = 0; i < k; i++) {
    product *= nums[i];
  }
  return product;
}
```

**Two-Pointer String** (O(n))

*Use for in-place string manipulation or when processing from both ends. Avoids expensive string concatenation. One pointer tracks position in result, another tracks boundaries of elements to process.*

```cpp
string reverseWords(string s) {
  string result;
  int n = s.length();
  int r = n - 1;

  for (int i = n - 1; i >= 0; i--) {
    if (s[i] == ' ' || i == 0) {
      int start = (i == 0) ? i : i + 1;
      for (int j = start; j <= r; j++) {
        result += s[j];
      }
      if (i != 0) result += ' ';
      r = i - 1;
    }
  }
  return result;
}
```

**Pitfalls:** Empty arrays, off-by-one, string immutability

---

### 2. TREES & GRAPHS

**Signals:**
- Hierarchical data (parent-child relationships, organizational charts)
- Explore paths/levels (root to leaf, level order traversal)
- Optimal path finding (shortest path, minimum cost path)
- Connectivity problems (islands, connected components, is reachable)
- Problems mention "tree", "graph", "node", "traversal", "path"
- Questions about "depth", "height", "diameter", "ancestors"
- Grid/matrix exploration (maze, flood fill, word search)

**Patterns:**

**DFS Path Sum** (O(n))

*Use for finding paths from root to leaves or computing aggregate values along paths. DFS explores each path fully before backtracking. Perfect for "maximum/minimum path sum" problems.*

```cpp
int maxPathSum(TreeNode* root) {
  if (!root) return 0;

  if (!root->left && !root->right)
    return root->val;

  int leftMax = root->left ? maxPathSum(root->left) : INT_MIN;
  int rightMax = root->right ? maxPathSum(root->right) : INT_MIN;

  return root->val + max(leftMax, rightMax);
}
```

**BFS Level-Order** (O(n))

*Use when you need to process nodes level-by-level or find shortest paths in unweighted graphs. Queue ensures nodes are visited in order of distance from root. Essential for "level order traversal" and "shortest path" problems.*

```cpp
vector<int> bfs(TreeNode* root) {
  vector<int> result;
  if (!root) return result;

  queue<TreeNode*> q;
  q.push(root);

  while (!q.empty()) {
    TreeNode* node = q.front();
    q.pop();
    result.push_back(node->val);

    if (node->left) q.push(node->left);
    if (node->right) q.push(node->right);
  }

  return result;
}
```

**DFS Grid/Maze** (O(rows × cols))

*Use for exploring 2D grids, finding paths in mazes, or counting connected components. Mark cells as visited to avoid cycles. The grid itself acts as the visited tracker by modifying cells in-place.*

```cpp
bool dfs(vector<vector<char>>& grid, int x, int y) {
  if (x < 0 || y < 0 || x >= n || y >= n ||
      grid[x][y] == 'W' || grid[x][y] == 'V')
    return false;

  if (x == targetX && y == targetY) return true;

  grid[x][y] = 'V'; // Mark visited

  return dfs(grid, x+1, y) || dfs(grid, x-1, y) ||
         dfs(grid, x, y+1) || dfs(grid, x, y-1);
}
```

**Pitfalls:** Forgetting visited marks, null handling, wrong traversal order

---

### 3. DYNAMIC PROGRAMMING

**Signals:**
- "Find optimal/best/minimum/maximum" (but with constraints)
- Subproblems overlap (solving same subproblem multiple times)
- Counting combinations/partitions (how many ways, total possibilities)
- Problems with "can you make", "ways to reach", "fewest steps"
- Optimization problems with choices at each step
- Knapsack variants (0/1, unbounded, subset sum)
- String matching/editing (edit distance, longest common subsequence)
- Fibonacci-like recurrences

**Patterns:**

**Combination Sum** (O(2^n))

*Use when you must select exactly k items from n to optimize some value. The mask pattern generates all k-combinations efficiently. For smaller k, this beats backtracking. Use prev_permutation on a boolean mask.*

```cpp
int bestSum(int target, int k, vector<int>& nums) {
  int best = -1;
  vector<bool> mask(nums.size(), false);
  fill(mask.begin(), mask.begin() + k, true);

  do {
    int sum = 0;
    for (size_t i = 0; i < nums.size(); i++)
      if (mask[i]) sum += nums[i];
    if (sum <= target && sum > best) best = sum;
  } while (prev_permutation(mask.begin(), mask.end()));

  return best;
}
```

**Partition Counting** (O(n²))

*Use for counting ways to partition integers or make change. The pentagonal number theorem provides an efficient recurrence. dp[i] represents count for value i. Build up from base case dp[0] = 1.*

```cpp
long long countPartitions(int n) {
  vector<long long> dp(n + 1, 0);
  dp[0] = 1;

  for (int i = 1; i <= n; i++) {
    for (int k = 1;; k++) {
      int p1 = k * (3*k - 1) / 2;
      int p2 = k * (3*k + 1) / 2;
      if (p1 > i && p2 > i) break;

      if (p1 <= i) dp[i] += (k % 2 ? dp[i-p1] : -dp[i-p1]);
      if (p2 <= i) dp[i] += (k % 2 ? dp[i-p2] : -dp[i-p2]);
    }
  }
  return dp[n];
}
```

**Pitfalls:** Wrong base cases, iteration order, overflow

---

### 4. BACKTRACKING & RECURSION

**Signals:**
- Generate all permutations/combinations (need to explore all possibilities)
- Constraint satisfaction (N-Queens, Sudoku, valid arrangement)
- "Find one valid solution" (not optimal, just valid)
- Problems asking for "all possible", "generate all", "list all"
- Combinatorial explosion (factorial/exponential possibilities)
- Must satisfy multiple constraints simultaneously
- Trial and error with ability to undo (place/remove, mark/unmark)

**Patterns:**

**Permutation Generation** (O(n!))

*Use when you need all permutations of a sequence. Swap elements to generate different orderings, backtrack by swapping back. Use a set to skip duplicate characters at each position for deduplication.*

```cpp
void permute(string str, int start, vector<string>& result) {
  if (start == str.length()) {
    result.push_back(str);
    return;
  }

  unordered_set<char> used;
  for (int i = start; i < str.length(); i++) {
    if (used.count(str[i])) continue; // Skip duplicates
    used.insert(str[i]);

    swap(str[start], str[i]);
    permute(str, start + 1, result);
    swap(str[start], str[i]); // Backtrack
  }
}

vector<string> getPermutations(string str) {
  vector<string> result;
  permute(str, 0, result);
  return result;
}
```

**Backtracking with Constraints** (O(varies))

*Use for constraint satisfaction problems where you need to find one valid solution. Try each possibility, recurse if valid, undo (backtrack) if it doesn't work. Track used elements and current path state.*

```cpp
vector<int> findSequence(int n) {
  vector<int> path;
  vector<bool> used(n + 1, false);

  function<bool()> backtrack = [&]() -> bool {
    if (path.size() == n) return true;

    for (int i = 1; i <= n; i++) {
      if (!used[i] && isValid(path, i)) {
        path.push_back(i);
        used[i] = true;

        if (backtrack()) return true;

        path.pop_back();  // Backtrack
        used[i] = false;
      }
    }
    return false;
  };

  backtrack();
  return path;
}
```

**Pitfalls:** Forgetting to backtrack, not pruning, duplicates

---

### 5. GREEDY & INTERVALS

**Signals:**
- Merging overlapping intervals (meeting rooms, time ranges)
- Scheduling/resource allocation (tasks, jobs, rooms)
- Locally optimal choices lead to global optimum
- Problems with "intervals", "ranges", "start/end times"
- "Minimum number of resources needed"
- Activity selection (maximum non-overlapping activities)
- Questions about overlaps, conflicts, or gaps

**Patterns:**

**Interval Merging** (O(n log n))

*Use for overlapping interval problems. Sort by start time, then merge overlapping intervals by extending the end. Track current interval and merge when next interval starts before current ends.*

```cpp
int sumIntervals(vector<pair<int,int>>& intervals) {
  if (intervals.empty()) return 0;

  sort(intervals.begin(), intervals.end());

  int total = 0, start = intervals[0].first, end = intervals[0].second;

  for (const auto& [a, b] : intervals) {
    if (a <= end) {
      end = max(end, b);  // Merge
    } else {
      total += end - start;
      start = a;
      end = b;
    }
  }

  return total + (end - start);
}
```

**Priority Queue Scheduling** (O(n log n))

*Use for scheduling problems with overlapping events. Min heap tracks when resources become available. Sort events by start time, reuse resources that finish before next event starts.*

```cpp
vector<int> allocateRooms(vector<pair<int,int>>& events) {
  sort(events.begin(), events.end());

  priority_queue<pair<int,int>, vector<pair<int,int>>,
                 greater<pair<int,int>>> heap; // min heap
  vector<int> allocations;
  int roomNum = 0;

  for (const auto& [start, end] : events) {
    if (!heap.empty() && heap.top().first < start) {
      int roomId = heap.top().second;
      heap.pop();
      allocations.push_back(roomId);
      heap.push({end, roomId});
    } else {
      allocations.push_back(++roomNum);
      heap.push({end, roomNum});
    }
  }

  return allocations;
}
```

**Pitfalls:** Not sorting first, wrong overlap check (<=), forgetting last interval

---

### 6. MATH & NUMBER THEORY

**Signals:**
- Primes, factors, divisibility (factorization, prime checking)
- GCD/LCM (simplification, least common multiple)
- Modular arithmetic (large numbers, remainders, cycles)
- Problems with "divisible by", "prime", "factors", "multiples"
- Large exponents (a^b where b is huge)
- Number patterns, sequences (Fibonacci, factorials, combinatorics)
- Digit manipulation (sum of digits, reverse number)

**Patterns:**

**Prime Factorization** (O(√n))

*Use when you need to break a number into prime factors. Only check up to √n since factors come in pairs. Count each prime's occurrence with a while loop, dividing n each time.*

```cpp
string primeFactors(int n) {
  string result;
  for (int i = 2; i * i <= n; i++) {
    if (n % i == 0) {
      int count = 0;
      while (n % i == 0) { n /= i; count++; }
      result += "(" + to_string(i);
      if (count > 1) result += "**" + to_string(count);
      result += ")";
    }
  }
  if (n > 1) result += "(" + to_string(n) + ")";
  return result;
}
```

**GCD Euclidean** (O(log min(a,b)))

*Use for finding greatest common divisor. Euclidean algorithm repeatedly replaces larger number with remainder until one becomes zero. Fundamental for fraction simplification and LCM calculation.*

```cpp
long long gcd(long long a, long long b) {
  while (b) {
    long long temp = b;
    b = a % b;
    a = temp;
  }
  return a;
}
```

**Modular Exponentiation** (O(log n))

*Use for computing (base^exp) % mod efficiently, avoiding overflow. Square the base and halve the exponent each iteration. Essential for cryptography and large number problems.*

```cpp
long long modPow(long long base, long long exp, long long mod) {
  long long result = 1;
  base %= mod;

  while (exp > 0) {
    if (exp % 2) result = (result * base) % mod;
    base = (base * base) % mod;
    exp /= 2;
  }
  return result;
}
```

**Pitfalls:** Overflow, edge cases (0, 1, negatives), i*i <= n not i <= sqrt(n)

---

### 7. BIT MANIPULATION

**Signals:**
- XOR properties (unique elements, pairs that cancel)
- Setting/clearing/toggling bits (flags, permissions)
- Counting set bits (Hamming weight, binary representation)
- Problems mention "binary", "bits", "powers of 2"
- Finding single element when others appear twice
- Checking if number is power of 2 (n & (n-1) == 0)
- Compact representation of sets (bitmask DP)
- Bitwise operations for optimization (& | ^ ~ << >>)

**Patterns:**

**XOR for Odd Occurrence** (O(n))

*Use when finding unique elements where others appear in pairs. XOR has property: a ^ a = 0 and a ^ 0 = a. All pairs cancel out, leaving only the unique element.*

```cpp
int findOdd(vector<int>& arr) {
  int result = 0;
  for (int n : arr) {
    result ^= n;
  }
  return result;
}
```

**Bit Setting** (O(1))

*Use for efficient flag management or compact data storage. Bit operations are faster than array lookups. OR sets, AND with NOT clears, XOR toggles.*

```cpp
int setBit(int num, int index) {
  return num | (1 << index);
}

int clearBit(int num, int index) {
  return num & ~(1 << index);
}

int toggleBit(int num, int index) {
  return num ^ (1 << index);
}
```

**Count Set Bits** (O(log n))

*Use when you need to count 1s in binary representation. Shift right and check LSB each iteration. __builtin_popcount() is optimized but understanding the manual method is important.*

```cpp
int countBits(int n) {
  int count = 0;
  while (n) {
    count += n & 1;
    n >>= 1;
  }
  return count;
  // Or: return __builtin_popcount(n);
}
```

**Pitfalls:** Signed vs unsigned, overflow, negative numbers

---

### 8. STACK/QUEUE

**Signals:**
- Matching pairs (parentheses, brackets, HTML tags)
- LIFO/FIFO ordering required (most recent first vs first-come first-served)
- Nearest smaller/larger element (stock span, next greater element)
- Problems with "valid", "balanced", "matching", "nested"
- Expression evaluation (infix, postfix, prefix)
- Undo/redo functionality
- Function call simulation (recursion to iteration conversion)

**Patterns:**

**Stack for Matching** (O(n))

*Use for matching nested structures (parentheses, brackets, tags). Push opening elements, pop and match on closing elements. Stack naturally handles LIFO ordering needed for nesting.*

```cpp
bool validParens(string str) {
  stack<char> s;
  unordered_map<char, char> pairs = {
    {')', '('}, {']', '['}, {'}', '{'}
  };

  for (char c : str) {
    if (c == '(' || c == '[' || c == '{') {
      s.push(c);
    } else {
      if (s.empty() || s.top() != pairs[c])
        return false;
      s.pop();
    }
  }

  return s.empty();
}
```

**Pitfalls:** Empty stack check, remaining items, wrong data structure

---

### 9. BINARY SEARCH

**Signals:**
- Sorted array or monotonic function
- "Find in O(log n)" time complexity requirement
- Search space divisible in half (can eliminate half each step)
- Problems asking for "first/last occurrence", "closest element"
- Finding threshold/boundary (smallest x where condition is true)
- Rotated sorted array variants
- Binary search on answer (minimize/maximize value that satisfies constraint)

**Patterns:**

**Classic Binary Search** (O(log n))

*Use on sorted arrays to find elements or boundaries. Halve search space each iteration. Use left + (right - left) / 2 to avoid overflow. Essential template for "find first/last occurrence" variants.*

```cpp
int binarySearch(vector<int>& arr, int target) {
  int left = 0, right = arr.size() - 1;

  while (left <= right) {
    int mid = left + (right - left) / 2;

    if (arr[mid] == target) return mid;
    else if (arr[mid] < target) left = mid + 1;
    else right = mid - 1;
  }

  return -1;
}
```

**Pitfalls:** Off-by-one (use <=), mid overflow, duplicates

---

### 10. LINKED LISTS

**Signals:**
- Linear structure with pointers (next, prev references)
- Cycle detection (circular references, infinite loops)
- Reversing/reordering (reverse in groups, rearrange)
- Problems mention "node", "linked list", "pointer"
- Finding middle element, k-th from end
- Merging sorted lists
- In-place manipulation without extra space
- Fast/slow pointer techniques (tortoise and hare)

**Patterns:**

**Floyd's Cycle Detection** (O(n) time, O(1) space)

*Use for detecting cycles in linked lists with constant space. Fast pointer moves twice as fast as slow. If they meet, there's a cycle. Then traverse the cycle to measure its length.*

```cpp
int detectCycle(Node* head) {
  Node *slow = head, *fast = head->next;

  while (fast && fast->next) {
    slow = slow->next;
    fast = fast->next->next;

    if (slow == fast) {
      // Cycle found, measure length
      int len = 0;
      do {
        slow = slow->next;
        len++;
      } while (slow != fast);
      return len;
    }
  }

  return 0;
}
```

**Pitfalls:** Null pointers, losing head reference, memory leaks

---

## Part II: Advanced Patterns

---

### 11. SLIDING WINDOW

**Signals:**
- Contiguous subarray/substring problems
- "Find longest/shortest substring with constraint"
- "Maximum/minimum sum of subarray of size k"
- "All subarrays that satisfy condition"
- Problems with "consecutive", "contiguous", "window"
- Optimization over all possible subarrays (but linear time)

**Patterns:**

**Fixed-Size Window** (O(n))

*Use when window size k is given. Slide window by removing leftmost element and adding new rightmost element. Maintain window state (sum, max, count, etc.) as you slide.*

```cpp
int maxSumSubarray(vector<int>& arr, int k) {
  int windowSum = 0, maxSum = 0;

  // Initial window
  for (int i = 0; i < k; i++) {
    windowSum += arr[i];
  }
  maxSum = windowSum;

  // Slide window
  for (int i = k; i < arr.size(); i++) {
    windowSum += arr[i] - arr[i - k];  // Add right, remove left
    maxSum = max(maxSum, windowSum);
  }

  return maxSum;
}
```

**Variable-Size Window** (O(n))

*Use when you need to find optimal window size. Expand window while valid, shrink when invalid. Two pointers: right expands, left contracts.*

```cpp
int longestSubstringKDistinct(string s, int k) {
  unordered_map<char, int> charCount;
  int left = 0, maxLen = 0;

  for (int right = 0; right < s.length(); right++) {
    charCount[s[right]]++;

    // Shrink window while invalid
    while (charCount.size() > k) {
      charCount[s[left]]--;
      if (charCount[s[left]] == 0) {
        charCount.erase(s[left]);
      }
      left++;
    }

    maxLen = max(maxLen, right - left + 1);
  }

  return maxLen;
}
```

**Pitfalls:** Forgetting to update window state, incorrect shrinking condition, off-by-one in window size

---

### 12. MONOTONIC STACK/QUEUE

**Signals:**
- "Next greater/smaller element"
- "Largest rectangle in histogram"
- "Sliding window maximum/minimum"
- Need to maintain order while removing elements
- Problems asking for "span", "nearest larger", "visible buildings"

**Patterns:**

**Next Greater Element** (O(n))

*Use stack to track indices of elements waiting for their next greater. Pop smaller elements when you find their answer.*

```cpp
vector<int> nextGreaterElement(vector<int>& nums) {
  int n = nums.size();
  vector<int> result(n, -1);
  stack<int> s;  // Monotonic decreasing stack (indices)

  for (int i = 0; i < n; i++) {
    // Pop smaller elements - found their next greater
    while (!s.empty() && nums[s.top()] < nums[i]) {
      result[s.top()] = nums[i];
      s.pop();
    }
    s.push(i);
  }

  return result;
}
```

**Sliding Window Maximum** (O(n))

*Use deque to maintain decreasing order. Front has max, remove elements outside window, remove smaller elements from back.*

```cpp
vector<int> maxSlidingWindow(vector<int>& nums, int k) {
  deque<int> dq;  // Stores indices
  vector<int> result;

  for (int i = 0; i < nums.size(); i++) {
    // Remove elements outside window
    while (!dq.empty() && dq.front() <= i - k) {
      dq.pop_front();
    }

    // Remove smaller elements (they'll never be max)
    while (!dq.empty() && nums[dq.back()] < nums[i]) {
      dq.pop_back();
    }

    dq.push_back(i);

    if (i >= k - 1) {
      result.push_back(nums[dq.front()]);
    }
  }

  return result;
}
```

**Pitfalls:** Stack stores indices not values, maintaining monotonic property, window boundary conditions

---

### 13. UNION FIND (DISJOINT SET)

**Signals:**
- Dynamic connectivity (are two elements connected?)
- Grouping elements into sets
- "Number of connected components"
- "Detect cycle in undirected graph"
- Problems about merging groups, friend circles, networks

**Patterns:**

**Path Compression + Union by Rank** (O(α(n)) ≈ O(1))

*Use for efficiently tracking connected components. Find() with path compression, Union() with rank optimization.*

```cpp
class UnionFind {
private:
  vector<int> parent;
  vector<int> rank;

public:
  UnionFind(int n) : parent(n), rank(n, 0) {
    for (int i = 0; i < n; i++) {
      parent[i] = i;
    }
  }

  int find(int x) {
    if (parent[x] != x) {
      parent[x] = find(parent[x]);  // Path compression
    }
    return parent[x];
  }

  bool unite(int x, int y) {
    int rootX = find(x);
    int rootY = find(y);

    if (rootX == rootY) return false;  // Already connected

    // Union by rank
    if (rank[rootX] < rank[rootY]) {
      parent[rootX] = rootY;
    } else if (rank[rootX] > rank[rootY]) {
      parent[rootY] = rootX;
    } else {
      parent[rootY] = rootX;
      rank[rootX]++;
    }

    return true;
  }

  bool connected(int x, int y) {
    return find(x) == find(y);
  }
};
```

**Pitfalls:** Forgetting path compression, not using union by rank, incorrect cycle detection

---

### 14. TRIE (PREFIX TREE)

**Signals:**
- Prefix matching, autocomplete
- "Words starting with..."
- "Search for word with wildcards"
- Dictionary operations (insert, search, startsWith)
- Multiple string searches with common prefixes

**Patterns:**

**Standard Trie** (Insert: O(m), Search: O(m) where m = word length)

*Use for efficient prefix operations. Each node has children for each character.*

```cpp
class TrieNode {
public:
  unordered_map<char, TrieNode*> children;
  bool isEndOfWord = false;
};

class Trie {
private:
  TrieNode* root;

public:
  Trie() {
    root = new TrieNode();
  }

  void insert(string word) {
    TrieNode* node = root;
    for (char c : word) {
      if (!node->children.count(c)) {
        node->children[c] = new TrieNode();
      }
      node = node->children[c];
    }
    node->isEndOfWord = true;
  }

  bool search(string word) {
    TrieNode* node = root;
    for (char c : word) {
      if (!node->children.count(c)) return false;
      node = node->children[c];
    }
    return node->isEndOfWord;
  }

  bool startsWith(string prefix) {
    TrieNode* node = root;
    for (char c : prefix) {
      if (!node->children.count(c)) return false;
      node = node->children[c];
    }
    return true;
  }
};
```

**Pitfalls:** Memory leaks, isEndOfWord vs children existence, case sensitivity

---

### 15. TOPOLOGICAL SORT

**Signals:**
- Task scheduling with dependencies
- "Course prerequisites" problems
- Directed Acyclic Graph (DAG) ordering
- "Detect cycle in directed graph"
- Build order, compilation order

**Patterns:**

**Kahn's Algorithm (BFS)** (O(V + E))

*Use for finding valid ordering of tasks. Track in-degrees, process nodes with in-degree 0.*

```cpp
vector<int> topologicalSort(int n, vector<pair<int,int>>& edges) {
  vector<vector<int>> graph(n);
  vector<int> inDegree(n, 0);

  // Build graph
  for (auto& [from, to] : edges) {
    graph[from].push_back(to);
    inDegree[to]++;
  }

  // Queue nodes with no dependencies
  queue<int> q;
  for (int i = 0; i < n; i++) {
    if (inDegree[i] == 0) {
      q.push(i);
    }
  }

  vector<int> result;
  while (!q.empty()) {
    int node = q.front();
    q.pop();
    result.push_back(node);

    // Reduce in-degree of neighbors
    for (int neighbor : graph[node]) {
      inDegree[neighbor]--;
      if (inDegree[neighbor] == 0) {
        q.push(neighbor);
      }
    }
  }

  // Check for cycle
  if (result.size() != n) {
    return {};  // Cycle detected
  }

  return result;
}
```

**DFS Approach** (O(V + E))

*Alternative using DFS. Detect cycles with three states: unvisited, visiting, visited.*

```cpp
bool dfsTopoSort(int node, vector<vector<int>>& graph,
                 vector<int>& state, vector<int>& result) {
  state[node] = 1;  // Visiting

  for (int neighbor : graph[node]) {
    if (state[neighbor] == 1) return false;  // Cycle
    if (state[neighbor] == 0 && !dfsTopoSort(neighbor, graph, state, result)) {
      return false;
    }
  }

  state[node] = 2;  // Visited
  result.push_back(node);
  return true;
}
```

**Pitfalls:** Detecting cycles, handling disconnected components, reversing DFS result

---

## Part III: Interview Strategy

---

## Quick Decision Tree

```
1. Can sort help? → Array/String or Greedy
2. Tree/Graph structure? → DFS/BFS
3. "Optimal" + overlapping? → DP
4. Generate all? → Backtracking
5. Intervals/scheduling? → Greedy + Sort
6. Numbers/divisibility? → Math
7. Find unique/XOR? → Bit Manipulation
8. Matching/nesting? → Stack
9. Sorted search? → Binary Search
10. Pointers/cycles? → Linked List (Floyd's)
```

---

## 20-Minute Strategy

| Time | Task |
|------|------|
| 0-2 min | Identify pattern from description |
| 2-5 min | Sketch approach, consider edge cases |
| 5-15 min | Code implementation |
| 15-18 min | Test with examples |
| 18-20 min | Debug and optimize |

### Red Flags (Rethink approach)
- Coding takes >10 minutes → wrong pattern
- Need >3 nested loops → reconsider
- Tests fail mysteriously → check edge cases first

---

## Complexity Cheat Sheet

| Pattern | Time | Space |
|---------|------|-------|
| Two Pointers | O(n) | O(1) |
| Binary Search | O(log n) | O(1) |
| Sorting | O(n log n) | O(1) to O(n) |
| DFS/BFS | O(V+E) | O(V) |
| DP 1D | O(n) | O(n) |
| DP 2D | O(n×m) | O(n×m) |
| Backtracking | O(2^n) to O(n!) | O(n) |
| Heap ops | O(log n) | O(n) |

---

## Common Edge Cases Checklist

- [ ] Empty input ([], "", null)
- [ ] Single element
- [ ] All same elements
- [ ] Already sorted/optimal
- [ ] Duplicates
- [ ] Negative numbers
- [ ] Zero
- [ ] Maximum/minimum values
- [ ] Odd/even length
- [ ] Overflow potential

---

## Part IV: Interview Excellence

---

## Interview Communication Framework

### The 5-Step Method

**1. CLARIFY (2 min)**
Ask these questions EVERY time:
- "What are the input constraints?" (array size, value ranges)
- "Can the input be empty or null?"
- "Are there duplicates?"
- "Is the input sorted?" (if array/list)
- "What should I return for edge cases?"
- "Are there any performance requirements?"

**2. EXAMPLES (1 min)**
Walk through examples:
- Normal case
- Edge case (empty, single element)
- Tricky case (duplicates, negatives)

**3. APPROACH (2 min)**
Think out loud:
- "I see this is a [pattern name] problem because..."
- "My initial thought is [brute force approach]"
- "That would be O(n²), but I can optimize using [data structure/algorithm]"
- "Let me sketch the algorithm..."

**4. COMPLEXITY ANALYSIS (30 sec)**
BEFORE coding:
- "Time complexity will be O(...) because..."
- "Space complexity will be O(...) for..."
- Get interviewer buy-in: "Does this sound good?"

**5. CODE (10-15 min)**
While coding:
- Announce what you're doing: "Now I'll handle the edge case..."
- Use meaningful variable names
- Add comments for tricky parts
- Walk through with an example as you go

**6. TEST (2-3 min)**
- Test with your examples
- Think of edge cases: "What if the array is empty?"
- Dry run through the code line by line

### Communication Templates

**Starting template:**
```
"Let me make sure I understand the problem correctly...
[Restate in your own words]
And the constraints are...
[List constraints]
For edge cases, should I...
[Ask about edge cases]
"
```

**Transitioning to solution:**
```
"I recognize this as a [pattern] problem.
The brute force would be [approach] with [complexity].
But we can optimize this to [complexity] using [technique].
Let me walk through my approach..."
```

**When stuck:**
```
"I'm thinking through a few approaches:
1. [Approach A] - [pros/cons]
2. [Approach B] - [pros/cons]
I'm leaning toward [choice] because...
What do you think?"
```

---

## Pattern Recognition Shortcuts

### "If You See This... Think That"

| Problem Phrase | Likely Pattern |
|----------------|----------------|
| "Subarray with sum/length" | Sliding Window, Prefix Sum |
| "Longest substring without repeating" | Sliding Window + HashMap |
| "Next greater/smaller" | Monotonic Stack |
| "K closest/largest/smallest" | Heap (Priority Queue) |
| "Shortest path" (unweighted) | BFS |
| "All paths", "Generate all" | Backtracking/DFS |
| "Number of ways to..." | DP (counting) |
| "Minimum/Maximum cost to..." | DP (optimization) |
| "Connected components" | Union Find or DFS |
| "Prerequisites/dependencies" | Topological Sort |
| "Substring/prefix matching" | Trie |
| "Palindrome substring" | Expand from center or DP |
| "Two sum / Three sum" | HashMap or Two Pointers |
| "Meeting rooms" | Interval Merging |
| "Cycle detection" | Floyd's or Union Find |

### Multiple Approaches Ranking

For many problems, multiple solutions exist. Rank by:

**Example: Two Sum**
1. ✅ **Optimal:** HashMap - O(n) time, O(n) space
2. ⚠️ **Acceptable:** Sort + Two Pointers - O(n log n) time, O(1) space
3. ❌ **Too Slow:** Brute Force - O(n²) time, O(1) space

**Example: Finding Duplicates**
1. ✅ **Optimal:** HashSet - O(n) time, O(n) space
2. ✅ **Space-optimized:** Sort - O(n log n) time, O(1) space
3. ✅ **Clever:** In-place marking (if allowed to modify) - O(n) time, O(1) space

---

## C++ STL Quick Reference

### Container Selection

| Need | Use | Why |
|------|-----|-----|
| Random access | `vector` | O(1) access, cache-friendly |
| Insert/delete at ends | `deque` | O(1) at both ends |
| Fast insert/delete anywhere | `list` | O(1) insert/delete, slow access |
| Unique elements | `unordered_set` | O(1) average search |
| Ordered elements | `set` | O(log n) search, sorted |
| Key-value pairs | `unordered_map` | O(1) average access |
| Ordered key-value | `map` | O(log n) access, sorted keys |
| Min/Max element | `priority_queue` | O(1) access, O(log n) insert |
| FIFO | `queue` | O(1) push/pop |
| LIFO | `stack` | O(1) push/pop |

### Common STL Patterns

**Heap Operations:**
```cpp
priority_queue<int> maxHeap;  // Max heap (default)
priority_queue<int, vector<int>, greater<int>> minHeap;  // Min heap

// Custom comparator
auto cmp = [](int a, int b) { return a > b; };
priority_queue<int, vector<int>, decltype(cmp)> pq(cmp);
```

**Sorting with Comparator:**
```cpp
sort(v.begin(), v.end());  // Ascending
sort(v.begin(), v.end(), greater<int>());  // Descending

// Custom
sort(v.begin(), v.end(), [](int a, int b) {
  return a > b;  // Your logic
});
```

**HashMap Patterns:**
```cpp
unordered_map<int, int> map;

// Check existence
if (map.count(key))  // Preferred for checking
if (map.find(key) != map.end())  // Returns iterator

// Safe access
map[key]++;  // Creates key with 0 if not exists
map.at(key);  // Throws exception if not exists
```

**Set Operations:**
```cpp
unordered_set<int> s;
s.insert(x);
s.erase(x);
s.count(x);  // 0 or 1

// Iteration
for (int x : s) { }
```

### Common Gotchas

❌ **Wrong:**
```cpp
vector<int> v;
int x = v[0];  // Undefined if empty
```
✅ **Right:**
```cpp
if (!v.empty()) int x = v[0];
```

❌ **Wrong:**
```cpp
for (int i = 0; i < v.size() - 1; i++)  // Underflow if size() == 0
```
✅ **Right:**
```cpp
for (int i = 0; i + 1 < v.size(); i++)
// or
if (v.size() >= 1) {
  for (int i = 0; i < v.size() - 1; i++)
}
```

❌ **Wrong:**
```cpp
map[key]++;  // Modifies map even if just checking
```
✅ **Right:**
```cpp
if (map.count(key)) map[key]++;
else map[key] = 1;
// or
map[key]++;  // If you want 0 initialization
```

---

## Debugging in Interviews

### Print Statement Strategy

```cpp
// Strategic prints
cout << "Input: "; printVector(nums); cout << endl;
cout << "After step 1: " << variable << endl;
cout << "Loop i=" << i << " j=" << j << " sum=" << sum << endl;
```

### Trace Through Method

Walk through code with example:
```
"Let me trace through with [1, 2, 3]:
- i=0, nums[0]=1, sum=1
- i=1, nums[1]=2, sum=3
- i=2, nums[2]=3, sum=6
- Return 6 ✓"
```

### Common Bugs by Pattern

| Pattern | Common Bug |
|---------|------------|
| Array | Off-by-one, empty array, index out of bounds |
| Sliding Window | Not updating window state, wrong shrink condition |
| DFS/BFS | Not marking visited, infinite loop |
| DP | Wrong base case, iteration order, not initializing |
| Binary Search | Infinite loop (mid calculation), <=  vs < |
| Backtracking | Forgetting to backtrack, not pruning |

---

## Practice Roadmap

### Master These 15 Problems

Learn these patterns, and you'll recognize 80% of interviews:

1. **Two Sum** (HashMap pattern)
2. **Valid Parentheses** (Stack pattern)
3. **Reverse Linked List** (Pointer manipulation)
4. **Maximum Subarray** (Kadane's algorithm)
5. **Climbing Stairs** (Basic DP)
6. **Binary Tree Level Order** (BFS pattern)
7. **Longest Substring Without Repeating** (Sliding Window)
8. **Number of Islands** (DFS/BFS on grid)
9. **Coin Change** (DP pattern)
10. **Merge Intervals** (Interval pattern)
11. **Top K Frequent Elements** (Heap pattern)
12. **Course Schedule** (Topological Sort)
13. **Word Search** (Backtracking)
14. **LRU Cache** (HashMap + DLL)
15. **Binary Search** (Search pattern)

### Progression Path

**Week 1-2: Foundations**
- Arrays, Strings, HashMaps
- Easy problems only
- Focus on: Two Sum, Valid Anagram, Contains Duplicate

**Week 3-4: Core Patterns**
- Trees, Linked Lists, Stack/Queue
- Mix of Easy/Medium
- Focus on: Valid Parentheses, Binary Tree Traversals, Reverse LL

**Week 5-6: Advanced Patterns**
- DP, Sliding Window, Greedy
- Medium problems
- Focus on: Climbing Stairs, Max Subarray, Longest Substring

**Week 7-8: Speed & Integration**
- All patterns mixed
- Timed practice (20 min per problem)
- Mock interviews

---

## System Design Context

These algorithms power real systems:

| Algorithm | Real-World Use |
|-----------|----------------|
| LRU Cache (HashMap + DLL) | Redis, CPU caches, browser caching |
| Trie | Autocomplete, spell checkers, IP routing |
| Union Find | Network connectivity, image processing |
| Topological Sort | Build systems, task schedulers |
| Dijkstra/BFS | Google Maps, network routing |
| Sliding Window | Rate limiting, log analysis |
| Bloom Filter | Database query optimization |
| Consistent Hashing | Load balancers, distributed caches |

**Interview Tip:** Mention real-world applications when relevant:
"This is similar to how an LRU cache works in Redis..."
