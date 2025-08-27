# Hints for Stock Trader

## Hint 1 (Reveal after 10 minutes)
You need to find the largest difference between two numbers where the smaller number comes before the larger number in the array.

## Hint 2 (Reveal after 15 minutes)
As you iterate through the array, what information would be useful to track?

## Hint 3 (Reveal after 20 minutes)
Keep track of the minimum price seen so far. For each price, calculate what profit you'd make if you sold at that price.

## Hint 4 (Reveal after 25 minutes)
You only need one pass through the array. Update two variables: minimum price so far and maximum profit so far.

## Solution Approach (Reveal only if stuck after 30 minutes)
1. Initialize min_price to first element (or i32::MAX)
2. Initialize max_profit to 0
3. Iterate through prices starting from index 1
4. For each price:
   - Update min_price if current price is lower
   - Calculate potential profit: current_price - min_price
   - Update max_profit if potential profit is higher
5. Return max_profit

Time: O(n), Space: O(1)