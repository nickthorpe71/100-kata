// Container Measure - Interview Problem
// Run with: cargo run (to see problem)
// Test with: cargo test (to validate solution)

fn main() {
    println!("🪣 PROBLEM: Container Measure");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!();
    println!("You are given an array of non-negative integers where each element represents");
    println!("the height of a vertical line on a chart. The array indices represent positions");
    println!("along the x-axis. Find two lines that together with the x-axis form a container");
    println!("that would hold the maximum amount of water.");
    println!();
    println!("Note: You cannot tilt the container.");
    println!();
    
    println!("📋 EXAMPLES:");
    println!();
    println!("Example 1:");
    println!("  Input: heights = [1, 8, 6, 2, 5, 4, 8, 3, 7]");
    println!("  Output: 49");
    println!("  Explanation: The lines at index 1 (height=8) and index 8 (height=7) form a container.");
    println!("               Area = min(8, 7) * (8 - 1) = 7 * 7 = 49");
    println!();
    
    println!("Example 2:");
    println!("  Input: heights = [1, 1]");
    println!("  Output: 1");
    println!("  Explanation: The two lines form a container with area = min(1, 1) * (1 - 0) = 1");
    println!();
    
    println!("Example 3:");
    println!("  Input: heights = [4, 3, 2, 1, 4]");
    println!("  Output: 16");
    println!("  Explanation: The lines at index 0 and index 4 form the maximum container.");
    println!("               Area = min(4, 4) * (4 - 0) = 4 * 4 = 16");
    println!();
    
    println!("⚡ CONSTRAINTS:");
    println!("  • 2 <= heights.length <= 10^5");
    println!("  • 0 <= heights[i] <= 10^4");
    println!();
    
    println!("💡 FOLLOW-UP QUESTIONS:");
    println!("  1. What's the time complexity of your solution?");
    println!("  2. Can you solve it in O(n) time?");
    println!("  3. What about finding the k containers with maximum water?");
    println!();
    
    println!("🚀 TO GET STARTED:");
    println!("  1. Implement the find_max_water_container function below");
    println!("  2. Run 'cargo test' to validate your solution");
    println!("  3. All tests should pass!");
    println!();
    
    // Demo run
    let example = vec![1, 8, 6, 2, 5, 4, 8, 3, 7];
    println!("🧪 DEMO RUN:");
    println!("  find_max_water_container({:?}) = {}", example, find_max_water_container(example.clone()));
}

/// Find the maximum amount of water a container can hold
/// 
/// # Arguments
/// * `heights` - Vector of non-negative integers representing line heights
/// 
/// # Returns
/// * `i32` - Maximum area of water that can be contained
///
/// # R* Style Notes
/// - Area = min(height[left], height[right]) * (right - left)
/// - Consider both brute force and optimized approaches
/// - Two-pointer technique can solve in O(n) time
/// - Use descriptive variable names like `left_pointer`, `right_pointer`
fn find_max_water_container(heights: Vec<i32>) -> i32 {
    // TODO: Implement your solution here
    //
    // Hints for R* style:
    // - Brute force: Check all pairs O(n²)
    // - Optimal: Two pointers from ends, move the shorter one inward
    // - Why move shorter? Moving taller can't increase area (width decreases, height limited by shorter)
    // - Track max_area seen so far
    // - Area formula: min(heights[left], heights[right]) * (right - left)
    
    unimplemented!("Replace this with your solution")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_1() {
        let heights = vec![1, 8, 6, 2, 5, 4, 8, 3, 7];
        assert_eq!(find_max_water_container(heights), 49);
    }

    #[test]
    fn test_example_2() {
        let heights = vec![1, 1];
        assert_eq!(find_max_water_container(heights), 1);
    }

    #[test]
    fn test_example_3() {
        let heights = vec![4, 3, 2, 1, 4];
        assert_eq!(find_max_water_container(heights), 16);
    }

    #[test]
    fn test_ascending_heights() {
        let heights = vec![1, 2, 3, 4, 5];
        // Best container: heights[0]=1, heights[4]=5, area = min(1,5) * (4-0) = 4
        assert_eq!(find_max_water_container(heights), 4);
    }

    #[test]
    fn test_descending_heights() {
        let heights = vec![5, 4, 3, 2, 1];
        // Best container: heights[0]=5, heights[4]=1, area = min(5,1) * (4-0) = 4
        assert_eq!(find_max_water_container(heights), 4);
    }

    #[test]
    fn test_equal_heights() {
        let heights = vec![3, 3, 3, 3];
        // Best container: any two at max distance, area = min(3,3) * (3-0) = 9
        assert_eq!(find_max_water_container(heights), 9);
    }

    #[test]
    fn test_zero_heights() {
        let heights = vec![0, 5, 0];
        // Best container involves middle element with either side: area = min(0,5) * 2 = 0
        assert_eq!(find_max_water_container(heights), 0);
    }

    #[test]
    fn test_mountain_pattern() {
        let heights = vec![1, 3, 5, 4, 2];
        // Test various combinations to find max
        assert_eq!(find_max_water_container(heights), 6); // heights[1]=3, heights[4]=2, area = min(3,2) * (4-1) = 6
    }

    #[test]
    fn test_valley_pattern() {
        let heights = vec![5, 1, 2, 1, 5];
        // Best container: heights[0]=5, heights[4]=5, area = min(5,5) * (4-0) = 20
        assert_eq!(find_max_water_container(heights), 20);
    }

    #[test]
    fn test_two_tall_towers() {
        let heights = vec![10, 1, 1, 1, 10];
        // Best container: heights[0]=10, heights[4]=10, area = min(10,10) * (4-0) = 40
        assert_eq!(find_max_water_container(heights), 40);
    }

    #[test]
    fn test_minimum_input() {
        let heights = vec![2, 1];
        // Only one possible container: area = min(2,1) * (1-0) = 1
        assert_eq!(find_max_water_container(heights), 1);
    }

    #[test]
    fn test_large_numbers() {
        let heights = vec![10000, 1, 10000];
        // Best container: heights[0]=10000, heights[2]=10000, area = min(10000,10000) * (2-0) = 20000
        assert_eq!(find_max_water_container(heights), 20000);
    }
}