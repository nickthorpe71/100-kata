// Triple Finder - Interview Problem
// Run with: cargo run (to see problem)
// Test with: cargo test (to validate solution)

fn main() {
    println!("🎯 PROBLEM: Triple Finder");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━");
    println!();
    println!("Given an array of integers, find all unique triplets in the array which give");
    println!("the sum of zero.");
    println!();
    println!("Note: The solution set must not contain duplicate triplets.");
    println!();
    
    println!("📋 EXAMPLES:");
    println!();
    println!("Example 1:");
    println!("  Input: nums = [-1, 0, 1, 2, -1, -4]");
    println!("  Output: [[-1, -1, 2], [-1, 0, 1]]");
    println!("  Explanation: ");
    println!("    nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0");
    println!("    nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0");
    println!("    nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0");
    println!("    The distinct triplets are [-1, 0, 1] and [-1, -1, 2]");
    println!();
    
    println!("Example 2:");
    println!("  Input: nums = [0, 1, 1]");
    println!("  Output: []");
    println!("  Explanation: The only possible triplet does not sum to 0");
    println!();
    
    println!("Example 3:");
    println!("  Input: nums = [0, 0, 0]");
    println!("  Output: [[0, 0, 0]]");
    println!("  Explanation: The only possible triplet sums to 0");
    println!();
    
    println!("⚡ CONSTRAINTS:");
    println!("  • 3 <= nums.length <= 3000");
    println!("  • -10^5 <= nums[i] <= 10^5");
    println!();
    
    println!("💡 FOLLOW-UP QUESTIONS:");
    println!("  1. How do you handle duplicates?");
    println!("  2. What if you needed to find quadruplets?");
    println!("  3. What if the target sum was variable instead of zero?");
    println!();
    
    println!("🚀 TO GET STARTED:");
    println!("  1. Implement the find_zero_sum_triplets function below");
    println!("  2. Run 'cargo test' to validate your solution");
    println!("  3. All tests should pass!");
    println!();
    
    // Demo run
    let example = vec![-1, 0, 1, 2, -1, -4];
    println!("🧪 DEMO RUN:");
    println!("  find_zero_sum_triplets({:?}) = {:?}", example, find_zero_sum_triplets(example.clone()));
}

/// Find all unique triplets that sum to zero
/// 
/// # Arguments
/// * `nums` - Vector of integers to search in
/// 
/// # Returns
/// * `Vec<Vec<i32>>` - Vector of triplets where each triplet sums to zero
///
/// # R* Style Notes
/// - Sort first to enable two-pointer technique and handle duplicates
/// - For each element, find pairs that sum to its negative
/// - Skip duplicates at all positions to avoid duplicate triplets
/// - Use descriptive names like `left_pointer`, `right_pointer`
fn find_zero_sum_triplets(nums: Vec<i32>) -> Vec<Vec<i32>> {
    // TODO: Implement your solution here
    //
    // Hints for R* style:
    // - Step 1: Sort the array to enable two-pointer technique
    // - Step 2: For each nums[i], find pairs that sum to -nums[i]
    // - Step 3: Use two pointers (left=i+1, right=n-1) to find pairs
    // - Step 4: Skip duplicates at all three positions (i, left, right)
    // - Early termination: if nums[i] > 0, break (no more zero sums possible)
    // - Time: O(n²), Space: O(1) excluding output
    
    unimplemented!("Replace this with your solution")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sort_triplets(mut triplets: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
        // Sort each triplet and then sort the triplets for consistent comparison
        for triplet in &mut triplets {
            triplet.sort();
        }
        triplets.sort();
        triplets
    }

    #[test]
    fn test_example_1() {
        let nums = vec![-1, 0, 1, 2, -1, -4];
        let mut result = find_zero_sum_triplets(nums);
        let mut expected = vec![vec![-1, -1, 2], vec![-1, 0, 1]];
        
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_example_2() {
        let nums = vec![0, 1, 1];
        let result = find_zero_sum_triplets(nums);
        assert_eq!(result, Vec::<Vec<i32>>::new());
    }

    #[test]
    fn test_example_3() {
        let nums = vec![0, 0, 0];
        let result = find_zero_sum_triplets(nums);
        let expected = vec![vec![0, 0, 0]];
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_no_triplets() {
        let nums = vec![1, 2, 3];
        let result = find_zero_sum_triplets(nums);
        assert_eq!(result, Vec::<Vec<i32>>::new());
    }

    #[test]
    fn test_all_positive() {
        let nums = vec![1, 2, 3, 4, 5];
        let result = find_zero_sum_triplets(nums);
        assert_eq!(result, Vec::<Vec<i32>>::new());
    }

    #[test]
    fn test_all_negative() {
        let nums = vec![-1, -2, -3, -4, -5];
        let result = find_zero_sum_triplets(nums);
        assert_eq!(result, Vec::<Vec<i32>>::new());
    }

    #[test]
    fn test_mixed_with_zero() {
        let nums = vec![-1, 0, 1];
        let result = find_zero_sum_triplets(nums);
        let expected = vec![vec![-1, 0, 1]];
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_multiple_zeros() {
        let nums = vec![0, 0, 0, 0];
        let result = find_zero_sum_triplets(nums);
        let expected = vec![vec![0, 0, 0]]; // Should only have one unique triplet
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_duplicate_handling() {
        let nums = vec![-1, 0, 1, 2, -1, -4, -1, 0, 1];
        let result = find_zero_sum_triplets(nums);
        // Should not have duplicate triplets
        let mut unique_check = result.clone();
        unique_check.sort();
        unique_check.dedup();
        assert_eq!(result.len(), unique_check.len(), "Result contains duplicate triplets");
    }

    #[test]
    fn test_large_values() {
        let nums = vec![-100000, 50000, 50000];
        let result = find_zero_sum_triplets(nums);
        let expected = vec![vec![-100000, 50000, 50000]];
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_minimum_size() {
        let nums = vec![1, -1, 0];
        let result = find_zero_sum_triplets(nums);
        let expected = vec![vec![-1, 0, 1]];
        assert_eq!(sort_triplets(result), sort_triplets(expected));
    }

    #[test]
    fn test_complex_case() {
        let nums = vec![-4, -2, -1, 0, 1, 2, 3, 4];
        let result = find_zero_sum_triplets(nums);
        // Should find multiple valid triplets like [-4, 1, 3], [-2, -1, 3], etc.
        // Verify all triplets sum to zero
        for triplet in &result {
            let sum: i32 = triplet.iter().sum();
            assert_eq!(sum, 0, "Triplet {:?} does not sum to zero", triplet);
        }
        // Should have at least a few triplets
        assert!(result.len() >= 3);
    }

    #[test]
    fn test_all_triplets_sum_to_zero() {
        // Verify all returned triplets actually sum to zero
        let nums = vec![-1, 0, 1, 2, -1, -4];
        let result = find_zero_sum_triplets(nums);
        
        for triplet in result {
            assert_eq!(triplet.len(), 3, "Each result should be a triplet");
            let sum: i32 = triplet.iter().sum();
            assert_eq!(sum, 0, "Triplet {:?} does not sum to zero", triplet);
        }
    }
}