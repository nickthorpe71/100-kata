# Hints for Pair Finder

## Hint 1 (Reveal after 10 minutes)
Think about what information you need to track as you iterate through the array.

## Hint 2 (Reveal after 15 minutes)
For each number, you know what its complement would need to be. Can you check if you've seen that complement before?

## Hint 3 (Reveal after 20 minutes)
Consider using a data structure that allows O(1) lookups.

## Hint 4 (Reveal after 25 minutes)
A HashMap can store numbers you've seen and their indices. For each new number, check if (target - number) exists in the map.

## Solution Approach (Reveal only if stuck after 30 minutes)
1. Create a HashMap to store (value, index) pairs
2. Iterate through the array
3. For each number, calculate complement = target - number
4. Check if complement exists in HashMap
5. If yes, return [map[complement], current_index]
6. If no, add current number and index to HashMap
7. Continue until solution found

Time: O(n), Space: O(n)