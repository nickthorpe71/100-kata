// Stock Trader - Interview Problem
// Run with: cargo run (to see problem)
// Test with: cargo test (to validate solution)

fn main() {
    println!("📈 PROBLEM: Stock Trader");
    println!("━━━━━━━━━━━━━━━━━━━━━━━");
    println!();
    println!("You are given an array where the i-th element represents the price of a stock on day i.");
    println!("You want to maximize your profit by choosing a single day to buy one stock and");
    println!("choosing a different day in the future to sell that stock.");
    println!();
    println!("Return the maximum profit you can achieve from this transaction.");
    println!("If you cannot achieve any profit, return 0.");
    println!();
    
    println!("📋 EXAMPLES:");
    println!();
    println!("Example 1:");
    println!("  Input: prices = [7, 1, 5, 3, 6, 4]");
    println!("  Output: 5");
    println!("  Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6), profit = 6 - 1 = 5");
    println!();
    
    println!("Example 2:");
    println!("  Input: prices = [7, 6, 4, 3, 1]");
    println!("  Output: 0");
    println!("  Explanation: No profit can be made, prices only decrease");
    println!();
    
    println!("Example 3:");
    println!("  Input: prices = [2, 4, 1]");
    println!("  Output: 2");
    println!("  Explanation: Buy on day 1 (price = 2) and sell on day 2 (price = 4), profit = 4 - 2 = 2");
    println!();
    
    println!("⚡ CONSTRAINTS:");
    println!("  • 1 <= prices.length <= 10^5");
    println!("  • 0 <= prices[i] <= 10^4");
    println!();
    
    println!("💡 FOLLOW-UP QUESTIONS:");
    println!("  1. What if you could complete multiple transactions?");
    println!("  2. What if there was a transaction fee?");
    println!("  3. What if you had a cooldown period after selling?");
    println!();
    
    println!("🚀 TO GET STARTED:");
    println!("  1. Implement the calculate_max_profit function below");
    println!("  2. Run 'cargo test' to validate your solution");
    println!("  3. All tests should pass!");
    println!();
    
    // Demo run
    let example = vec![7, 1, 5, 3, 6, 4];
    println!("🧪 DEMO RUN:");
    println!("  calculate_max_profit({:?}) = {}", example, calculate_max_profit(example));
}

/// Calculate the maximum profit from buying and selling stock once
/// 
/// # Arguments
/// * `prices` - Vector of stock prices where prices[i] is the price on day i
/// 
/// # Returns
/// * `i32` - Maximum profit achievable, or 0 if no profit possible
///
/// # R* Style Notes
/// - Think about what information you need to track as you iterate
/// - Can you solve this in one pass through the array?
/// - Consider the relationship between min price seen and current price
/// - Use descriptive variable names like `min_price_so_far`
fn calculate_max_profit(prices: Vec<i32>) -> i32 {
    // TODO: Implement your solution here
    //
    // Hints for R* style:
    // - You need to find max(prices[j] - prices[i]) where j > i
    // - Track minimum price seen so far
    // - Track maximum profit seen so far
    // - Single pass O(n) solution exists
    
    unimplemented!("Replace this with your solution")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_example_1() {
        let prices = vec![7, 1, 5, 3, 6, 4];
        assert_eq!(calculate_max_profit(prices), 5);
    }

    #[test]
    fn test_example_2() {
        let prices = vec![7, 6, 4, 3, 1];
        assert_eq!(calculate_max_profit(prices), 0);
    }

    #[test]
    fn test_example_3() {
        let prices = vec![2, 4, 1];
        assert_eq!(calculate_max_profit(prices), 2);
    }

    #[test]
    fn test_single_day() {
        let prices = vec![5];
        assert_eq!(calculate_max_profit(prices), 0);
    }

    #[test]
    fn test_two_days_profit() {
        let prices = vec![1, 5];
        assert_eq!(calculate_max_profit(prices), 4);
    }

    #[test]
    fn test_two_days_loss() {
        let prices = vec![5, 1];
        assert_eq!(calculate_max_profit(prices), 0);
    }

    #[test]
    fn test_all_same_price() {
        let prices = vec![3, 3, 3, 3];
        assert_eq!(calculate_max_profit(prices), 0);
    }

    #[test]
    fn test_ascending_prices() {
        let prices = vec![1, 2, 3, 4, 5];
        assert_eq!(calculate_max_profit(prices), 4); // Buy at 1, sell at 5
    }

    #[test]
    fn test_descending_prices() {
        let prices = vec![5, 4, 3, 2, 1];
        assert_eq!(calculate_max_profit(prices), 0);
    }

    #[test]
    fn test_max_values() {
        let prices = vec![0, 10000];
        assert_eq!(calculate_max_profit(prices), 10000);
    }

    #[test]
    fn test_complex_pattern() {
        // Buy at 1, sell at 6 for profit of 5
        let prices = vec![3, 1, 4, 1, 5, 9, 2, 6];
        assert_eq!(calculate_max_profit(prices), 8); // Buy at 1, sell at 9
    }
}