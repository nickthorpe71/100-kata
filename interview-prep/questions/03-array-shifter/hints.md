# Hints for Array Shifter

## Hint 1 (Reveal after 10 minutes)
What happens when k is greater than the array length? How can you normalize k?

## Hint 2 (Reveal after 15 minutes)
Think about what happens when you reverse portions of an array. Can reversing help achieve rotation?

## Hint 3 (Reveal after 20 minutes)
Try reversing the entire array first. What do you notice about the positions of elements?

## Hint 4 (Reveal after 25 minutes)
After reversing the entire array, try reversing the first k elements, then the remaining elements.

## Solution Approach (Reveal only if stuck after 30 minutes)
Three approaches:

**Approach 1: Using extra space**
1. Create new array
2. Place each element at position (i + k) % n
Time: O(n), Space: O(n)

**Approach 2: Cyclic replacements**
1. Move elements in cycles
2. Track visited elements
Time: O(n), Space: O(1)

**Approach 3: Reverse three times (elegant)**
1. k = k % nums.length (handle k > length)
2. Reverse entire array
3. Reverse first k elements
4. Reverse remaining n-k elements
Time: O(n), Space: O(1)

Example for [1,2,3,4,5,6,7], k=3:
- Reverse all: [7,6,5,4,3,2,1]
- Reverse first 3: [5,6,7,4,3,2,1]
- Reverse last 4: [5,6,7,1,2,3,4]