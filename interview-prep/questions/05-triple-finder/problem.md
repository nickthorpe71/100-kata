# Triple Finder

## Problem Statement
Given an array of integers, find all unique triplets in the array which give the sum of zero.

Note: The solution set must not contain duplicate triplets.

## Examples

### Example 1
```
Input: nums = [-1, 0, 1, 2, -1, -4]
Output: [[-1, -1, 2], [-1, 0, 1]]
Explanation: 
nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0
nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0
nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0
The distinct triplets are [-1, 0, 1] and [-1, -1, 2]
```

### Example 2
```
Input: nums = [0, 1, 1]
Output: []
Explanation: The only possible triplet does not sum to 0
```

### Example 3
```
Input: nums = [0, 0, 0]
Output: [[0, 0, 0]]
Explanation: The only possible triplet sums to 0
```

## Constraints
- 3 <= nums.length <= 3000
- -10^5 <= nums[i] <= 10^5

## Function Signature
```rust
fn find_zero_sum_triplets(nums: Vec<i32>) -> Vec<Vec<i32>> {
    // Your code here
}
```

## Follow-up
- How do you handle duplicates?
- What if you needed to find quadruplets?
- What if the target sum was variable instead of zero?