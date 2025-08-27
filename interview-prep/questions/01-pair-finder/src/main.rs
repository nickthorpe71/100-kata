// Pair Finder - Interview Problem
// Run with: cargo run (to see problem)
// Test with: cargo test (to validate solution)

use std::collections::HashMap;

fn main() {
    println!("🎯 PROBLEM: Pair Finder");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━");
    println!();
    println!("You are given an array of integers and a target sum.");
    println!("Find two numbers in the array that add up to the target sum and return their indices.");
    println!();
    
    println!("📋 EXAMPLES:");
    println!();
    println!("Example 1:");
    println!("  Input: numbers = [2, 7, 11, 15], target = 9");
    println!("  Output: [0, 1]");
    println!("  Explanation: numbers[0] + numbers[1] = 2 + 7 = 9");
    println!();
    
    println!("Example 2:");
    println!("  Input: numbers = [3, 2, 4], target = 6");
    println!("  Output: [1, 2]");
    println!("  Explanation: numbers[1] + numbers[2] = 2 + 4 = 6");
    println!();
    
    println!("Example 3:");
    println!("  Input: numbers = [3, 3], target = 6");
    println!("  Output: [0, 1]");
    println!("  Explanation: numbers[0] + numbers[1] = 3 + 3 = 6");
    println!();
    
    println!("⚡ CONSTRAINTS:");
    println!("  • 2 <= numbers.length <= 10^4");
    println!("  • -10^9 <= numbers[i] <= 10^9"); 
    println!("  • -10^9 <= target <= 10^9");
    println!("  • Exactly one solution exists");
    println!();
    
    println!("💡 FOLLOW-UP:");
    println!("  Can you solve this in less than O(n²) time complexity?");
    println!();
    
    println!("🚀 TO GET STARTED:");
    println!("  1. Implement the find_pair function below");
    println!("  2. Run 'cargo test' to validate your solution");
    println!("  3. All tests should pass!");
    println!();
    
    println!("🧪 DEMO RUN (will panic until implemented):");
    println!("  find_pair([2, 7, 11, 15], 9) -> Expected: [0, 1]");
}

/// Find two numbers in the array that add up to target and return their indices
/// 
/// # Arguments
/// * `numbers` - Vector of integers to search in
/// * `target` - The target sum we're looking for
/// 
/// # Returns
/// * `Vec<usize>` - Vector containing exactly two indices where numbers[i] + numbers[j] = target
///
/// # R* Style Notes
/// - Use descriptive variable names
/// - Prefer functional style when clear
/// - No external dependencies
/// - Handle the problem efficiently
fn find_pair(_numbers: Vec<i32>, _target: i32) -> Vec<usize> {
    // iterate over list and for each value store in hashmap: value: index
    // iterate again and search for the match to each in our hash map
    let mut value_index_map: HashMap<i32, usize> = HashMap::new();

    for (index, &value) in _numbers.iter().enumerate() {
        value_index_map.insert(value, index);
    }

    for (index, &value) in _numbers.iter().enumerate() {
        let other_half = _target - value;
        if let Some(&other_index) = value_index_map.get(&other_half) {
            if other_index == index {
                continue;
            }
            return vec![index, other_index];
        }
    }

    vec![0,1]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_1() {
        let numbers = vec![2, 7, 11, 15];
        let target = 9;
        let result = find_pair(numbers.clone(), target);
        
        // Should return indices that sum to target
        assert_eq!(result.len(), 2);
        let expected_pairs = vec![vec![0, 1], vec![1, 0]]; // Both orders acceptable
        assert!(expected_pairs.contains(&result), "Expected [0,1] or [1,0], got {:?}", result);
    }

    #[test]
    fn test_example_2() {
        let numbers = vec![3, 2, 4];
        let target = 6;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        let expected_pairs = vec![vec![1, 2], vec![2, 1]];
        assert!(expected_pairs.contains(&result), "Expected [1,2] or [2,1], got {:?}", result);
    }

    #[test]
    fn test_example_3() {
        let numbers = vec![3, 3];
        let target = 6;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        let expected_pairs = vec![vec![0, 1], vec![1, 0]];
        assert!(expected_pairs.contains(&result), "Expected [0,1] or [1,0], got {:?}", result);
    }

    #[test]
    fn test_negative_numbers() {
        let numbers = vec![-3, 4, 3, 90];
        let target = 0;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        // -3 + 3 = 0, so indices 0 and 2
        let expected_pairs = vec![vec![0, 2], vec![2, 0]];
        assert!(expected_pairs.contains(&result), "Expected [0,2] or [2,0], got {:?}", result);
    }

    #[test]
    fn test_large_numbers() {
        let numbers = vec![1000000000, -1000000000];
        let target = 0;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        let expected_pairs = vec![vec![0, 1], vec![1, 0]];
        assert!(expected_pairs.contains(&result), "Expected [0,1] or [1,0], got {:?}", result);
    }

    #[test]
    fn test_values_actually_sum_to_target() {
        // Verify the indices actually point to values that sum to target
        let numbers = vec![2, 7, 11, 15];
        let target = 9;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        let sum = numbers[result[0]] + numbers[result[1]];
        assert_eq!(sum, target, "Values at returned indices don't sum to target");
    }

    #[test]
    fn test_no_duplicate_indices() {
        // Ensure the same index isn't used twice
        let numbers = vec![3, 2, 4];
        let target = 6;
        let result = find_pair(numbers.clone(), target);
        
        assert_eq!(result.len(), 2);
        assert_ne!(result[0], result[1], "Cannot use the same index twice");
    }
}