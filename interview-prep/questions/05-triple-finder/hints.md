# Hints for Triple Finder

## Hint 1 (Reveal after 10 minutes)
This problem builds upon a simpler problem. Can you reduce it to finding pairs with a specific sum?

## Hint 2 (Reveal after 15 minutes)
Sorting the array first can help you avoid duplicates and optimize your search.

## Hint 3 (Reveal after 20 minutes)
Fix one element and find two other elements that sum to the negative of the fixed element.

## Hint 4 (Reveal after 25 minutes)
After sorting, for each element, use two pointers to find pairs that sum to its negative.

## Solution Approach (Reveal only if stuck after 30 minutes)
1. Sort the array first: O(n log n)
2. Iterate through array with index i from 0 to n-3
3. Skip duplicate values for i (if nums[i] == nums[i-1], continue)
4. For each nums[i], find pairs that sum to -nums[i]:
   - Use two pointers: left = i+1, right = n-1
   - While left < right:
     - Calculate sum = nums[i] + nums[left] + nums[right]
     - If sum == 0: 
       - Add triplet to result
       - Skip duplicates for both left and right pointers
       - Move both pointers inward
     - If sum < 0: move left pointer right
     - If sum > 0: move right pointer left

Key insights:
- Sorting helps avoid duplicates and enables two-pointer technique
- Skip duplicates at all three positions (i, left, right)
- Early termination: if nums[i] > 0 after sorting, no more zero sums possible

Time: O(n²), Space: O(1) excluding output