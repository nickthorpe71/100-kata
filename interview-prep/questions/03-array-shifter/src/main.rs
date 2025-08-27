// Array Shifter - Interview Problem
// Run with: cargo run (to see problem)
// Test with: cargo test (to validate solution)

fn main() {
    println!("🔄 PROBLEM: Array Shifter");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!();
    println!("Given an array, rotate the array to the right by k steps, where k is non-negative.");
    println!();
    
    println!("📋 EXAMPLES:");
    println!();
    println!("Example 1:");
    println!("  Input: nums = [1, 2, 3, 4, 5, 6, 7], k = 3");
    println!("  Output: [5, 6, 7, 1, 2, 3, 4]");
    println!("  Explanation: ");
    println!("    rotate 1 step to the right: [7, 1, 2, 3, 4, 5, 6]");
    println!("    rotate 2 steps to the right: [6, 7, 1, 2, 3, 4, 5]");
    println!("    rotate 3 steps to the right: [5, 6, 7, 1, 2, 3, 4]");
    println!();
    
    println!("Example 2:");
    println!("  Input: nums = [-1, -100, 3, 99], k = 2");
    println!("  Output: [3, 99, -1, -100]");
    println!("  Explanation:");
    println!("    rotate 1 step to the right: [99, -1, -100, 3]");
    println!("    rotate 2 steps to the right: [3, 99, -1, -100]");
    println!();
    
    println!("Example 3:");
    println!("  Input: nums = [1, 2], k = 3");
    println!("  Output: [2, 1]");
    println!("  Explanation: k = 3 is equivalent to k = 1 for an array of length 2");
    println!();
    
    println!("⚡ CONSTRAINTS:");
    println!("  • 1 <= nums.length <= 10^5");
    println!("  • -2^31 <= nums[i] <= 2^31 - 1");
    println!("  • 0 <= k <= 10^5");
    println!();
    
    println!("💡 FOLLOW-UP QUESTIONS:");
    println!("  1. Try to come up with as many solutions as you can");
    println!("  2. Could you do it in-place with O(1) extra space?");
    println!();
    
    println!("🚀 TO GET STARTED:");
    println!("  1. Implement the rotate_array function below");
    println!("  2. Run 'cargo test' to validate your solution");
    println!("  3. All tests should pass!");
    println!();
    
    // Demo run
    let mut example = vec![1, 2, 3, 4, 5, 6, 7];
    let k = 3;
    println!("🧪 DEMO RUN:");
    println!("  Before: {:?}", example);
    rotate_array(&mut example, k);
    println!("  After rotate_array(nums, {k}): {:?}", example);
}

/// Rotate the array to the right by k steps
/// 
/// # Arguments
/// * `nums` - Mutable reference to vector of integers to rotate
/// * `k` - Number of steps to rotate right (non-negative)
/// 
/// # Returns
/// * Nothing - modifies the vector in place
///
/// # R* Style Notes
/// - Consider what happens when k > array length
/// - Think about the relationship between reversing and rotating
/// - Multiple approaches exist: extra space vs in-place
/// - Use descriptive variable names
fn rotate_array(nums: &mut Vec<i32>, k: i32) {
    // TODO: Implement your solution here
    // 
    // Hints for R* style:
    // - What happens when k >= nums.len()? Use modulo
    // - Approach 1: Create new array and place elements at (i + k) % n
    // - Approach 2: Reverse entire array, then reverse first k, then reverse rest
    // - Approach 3: Use cyclic replacements
    // - Think about edge cases: empty array, k=0, k > length
    
    unimplemented!("Replace this with your solution")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_1() {
        let mut nums = vec![1, 2, 3, 4, 5, 6, 7];
        rotate_array(&mut nums, 3);
        assert_eq!(nums, vec![5, 6, 7, 1, 2, 3, 4]);
    }

    #[test]
    fn test_example_2() {
        let mut nums = vec![-1, -100, 3, 99];
        rotate_array(&mut nums, 2);
        assert_eq!(nums, vec![3, 99, -1, -100]);
    }

    #[test]
    fn test_example_3() {
        let mut nums = vec![1, 2];
        rotate_array(&mut nums, 3);
        assert_eq!(nums, vec![2, 1]);
    }

    #[test]
    fn test_no_rotation() {
        let mut nums = vec![1, 2, 3, 4];
        rotate_array(&mut nums, 0);
        assert_eq!(nums, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_full_rotation() {
        let mut nums = vec![1, 2, 3, 4];
        rotate_array(&mut nums, 4);
        assert_eq!(nums, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_single_element() {
        let mut nums = vec![1];
        rotate_array(&mut nums, 5);
        assert_eq!(nums, vec![1]);
    }

    #[test]
    fn test_k_greater_than_length() {
        let mut nums = vec![1, 2, 3];
        rotate_array(&mut nums, 5); // Same as k = 2
        assert_eq!(nums, vec![2, 3, 1]);
    }

    #[test]
    fn test_large_k() {
        let mut nums = vec![1, 2, 3, 4, 5];
        rotate_array(&mut nums, 12); // Same as k = 2
        assert_eq!(nums, vec![4, 5, 1, 2, 3]);
    }

    #[test]
    fn test_reverse_rotation_pattern() {
        // Test the three-reverse approach pattern
        let mut nums = vec![1, 2, 3, 4, 5, 6];
        rotate_array(&mut nums, 2);
        assert_eq!(nums, vec![5, 6, 1, 2, 3, 4]);
    }

    #[test]
    fn test_negative_numbers() {
        let mut nums = vec![-5, -3, -1, 1, 3, 5];
        rotate_array(&mut nums, 3);
        assert_eq!(nums, vec![1, 3, 5, -5, -3, -1]);
    }

    #[test]
    fn test_two_elements() {
        let mut nums = vec![1, 2];
        rotate_array(&mut nums, 1);
        assert_eq!(nums, vec![2, 1]);
    }
}