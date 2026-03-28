#include <iostream>
#include <stdexcept>
#include <vector>
#include <unordered_map>

using namespace std;

int searchRotated(const std::vector<int>& nums, int target) {
    throw std::logic_error("Not implemented");
}

int kthLargest(const std::vector<int>& nums, int k) {
    throw std::logic_error("Not implemented");
}

std::vector<std::vector<int>> kClosest(const std::vector<std::vector<int>>& points, int k) {
    throw std::logic_error("Not implemented");
}

vector<int> topKFrequent(const vector<int>& nums, int k) {
    unordered_map<int, int> frequency;
    for (int i = 0; i < nums.size(); i++) {
        frequency[nums[i]]++;
    }

    vector<vector<int>> buckets(nums.size() + 1);
    for (auto& [num, count] : frequency) {
        buckets[count].push_back(num);
    }

    vector<int> result;

    for (int i = static_cast<int>(buckets.size()) - 1; i > 0; i--) {
        for (int num : buckets[i]) {
            result.push_back(num);
            if (result.size() == k) {
                return result;
            }
        }
    }

    return result;
}

int main() {
    std::cout << "Implement the three functions for kata 90 and add tests." << std::endl;
    return 0;
}
