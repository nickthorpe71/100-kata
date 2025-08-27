# Pair Finder

## Problem Statement
You are given an array of integers and a target sum. Find two numbers in the array that add up to the target sum and return their indices. 

## Examples

### Example 1
```
Input: numbers = [2, 7, 11, 15], target = 9
Output: [0, 1]
Explanation: numbers[0] + numbers[1] = 2 + 7 = 9
```

### Example 2
```
Input: numbers = [3, 2, 4], target = 6
Output: [1, 2]
Explanation: numbers[1] + numbers[2] = 2 + 4 = 6
```

### Example 3
```
Input: numbers = [3, 3], target = 6
Output: [0, 1]
Explanation: numbers[0] + numbers[1] = 3 + 3 = 6
```

## Constraints
- 2 <= numbers.length <= 10^4
- -10^9 <= numbers[i] <= 10^9
- -10^9 <= target <= 10^9
- Exactly one solution exists

## Function Signature
```rust
fn find_pair(numbers: Vec<i32>, target: i32) -> Vec<usize> {
    // Your code here
}
```

## Follow-up
Can you solve this in less than O(n²) time complexity?