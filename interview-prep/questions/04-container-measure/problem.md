# Container Measure

## Problem Statement
You are given an array of non-negative integers where each element represents the height of a vertical line on a chart. The array indices represent positions along the x-axis. Find two lines that together with the x-axis form a container that would hold the maximum amount of water.

Note: You cannot tilt the container.

## Examples

### Example 1
```
Input: heights = [1, 8, 6, 2, 5, 4, 8, 3, 7]
Output: 49
Explanation: The lines at index 1 (height=8) and index 8 (height=7) form a container.
Area = min(8, 7) * (8 - 1) = 7 * 7 = 49
```

### Example 2
```
Input: heights = [1, 1]
Output: 1
Explanation: The two lines form a container with area = min(1, 1) * (1 - 0) = 1
```

### Example 3
```
Input: heights = [4, 3, 2, 1, 4]
Output: 16
Explanation: The lines at index 0 and index 4 form the maximum container.
Area = min(4, 4) * (4 - 0) = 4 * 4 = 16
```

## Constraints
- 2 <= heights.length <= 10^5
- 0 <= heights[i] <= 10^4

## Function Signature
```rust
fn find_max_water_container(heights: Vec<i32>) -> i32 {
    // Your code here
}
```

## Follow-up
- What's the time complexity of your solution?
- Can you solve it in O(n) time?
- What about finding the k containers with maximum water?