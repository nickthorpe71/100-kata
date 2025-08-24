#include <vector>
#include <algorithm>
#include <iostream>

int sum_intervals(std::vector<std::pair<int, int>>& intervals) {
    if (intervals.empty()) return 0;

    // Sort intervals based on the start value
    std::sort(intervals.begin(), intervals.end());

    int total = 0;
    // Start with the first interval
    int start = intervals[0].first;
    int end = intervals[0].second;

    for (const auto& interval : intervals) {
        // If the current interval overlaps with the merged one, merge them
        if (interval.first <= end) {
            end = std::max(end, interval.second);
        } else {
            // No overlap, add the length of the merged interval to total and start a new merge
            total += end - start;
            start = interval.first;
            end = interval.second;
        }
    }

    // Add the length of the last merged interval
    total += end - start;
    return total;
}

int main() {
    // Test cases
    std::vector<std::pair<int, int>> intervals = {{1, 5}, {6, 10}};
    std::cout << sum_intervals(intervals) << '\n'; // should equal 8

    intervals = {{1, 4}, {7, 10}, {3, 5}};
    std::cout << sum_intervals(intervals) << '\n'; // should equal 7

    intervals = {{1, 5}, {10, 20}, {1, 6}, {16, 19}, {5, 11}};
    std::cout << sum_intervals(intervals) << '\n'; // should equal 19

    intervals = {{0, 20}, {-100000000, 10}, {30, 40}};
    std::cout << sum_intervals(intervals) << '\n'; // should equal 100000030

    return 0;
}