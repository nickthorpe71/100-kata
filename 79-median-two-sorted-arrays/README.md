# 79 Median Of Two Sorted Arrays

## Difficulty
Hard

## Format
Single hard problem with strong complexity expectations.

## Time Target
45-60 minutes.

## Prompt
Given two sorted arrays `nums1` and `nums2`, return the median of the combined sorted order.

The expected target is `O(log(min(m, n)))`.

## Function Signature

```cpp
double findMedianSortedArrays(const vector<int>& nums1, const vector<int>& nums2);
```

## Interview Focus
- partition-based binary search
- careful off-by-one handling
- explaining why the partition is valid
