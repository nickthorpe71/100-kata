# Hints for Container Measure

## Hint 1 (Reveal after 10 minutes)
The area of water is determined by the shorter line and the distance between the two lines.

## Hint 2 (Reveal after 15 minutes)
Start by considering the brute force approach. How would you check all possible pairs?

## Hint 3 (Reveal after 20 minutes)
Can you start with the widest container and then intelligently narrow your search?

## Hint 4 (Reveal after 25 minutes)
Use two pointers starting from both ends. Move the pointer pointing to the shorter line inward.

## Solution Approach (Reveal only if stuck after 30 minutes)
**Brute Force**: O(n²)
- Check all pairs of lines
- Track maximum area

**Optimal Two-Pointer Approach**: O(n)
1. Start with left pointer at index 0, right pointer at last index
2. Calculate current area: min(height[left], height[right]) * (right - left)
3. Update max_area if current is larger
4. Move the pointer pointing to the shorter line:
   - If height[left] < height[right], move left pointer right
   - Otherwise, move right pointer left
5. Repeat until pointers meet

Why this works:
- We start with maximum width
- Moving the shorter line might find a taller line that increases area
- Moving the taller line will only decrease area (width decreases, height can't increase beyond shorter line)

Time: O(n), Space: O(1)