# Stock Trader

## Problem Statement
You are given an array where the i-th element represents the price of a stock on day i. You want to maximize your profit by choosing a single day to buy one stock and choosing a different day in the future to sell that stock.

Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.

## Examples

### Example 1
```
Input: prices = [7, 1, 5, 3, 6, 4]
Output: 5
Explanation: Buy on day 2 (price = 1) and sell on day 5 (price = 6), profit = 6 - 1 = 5
```

### Example 2
```
Input: prices = [7, 6, 4, 3, 1]
Output: 0
Explanation: No profit can be made, prices only decrease
```

### Example 3
```
Input: prices = [2, 4, 1]
Output: 2
Explanation: Buy on day 1 (price = 2) and sell on day 2 (price = 4), profit = 4 - 2 = 2
```

## Constraints
- 1 <= prices.length <= 10^5
- 0 <= prices[i] <= 10^4

## Function Signature
```rust
fn calculate_max_profit(prices: Vec<i32>) -> i32 {
    // Your code here
}
```

## Follow-up Questions
1. What if you could complete multiple transactions?
2. What if there was a transaction fee?
3. What if you had a cooldown period after selling?