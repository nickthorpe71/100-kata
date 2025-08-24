#include <iostream>
#include <vector>
#include <algorithm>

class BestTravel
{
public:
    static int chooseBestSum(int t, int k, std::vector<int>& ls);
};

int BestTravel::chooseBestSum(int t, int k, std::vector<int>& ls)
{
    if (ls.size() < k)
    {
        return -1;
    }

    int best_sum = -1;
    
    // Create a mask to generate combinations
    std::vector<bool> mask(ls.size(), false);
    std::fill(mask.begin(), mask.begin() + k, true);

    do {
        int current_sum = 0;
        for (size_t i = 0; i < ls.size(); ++i) {
            if (mask[i]) {
                current_sum += ls[i];
            }
        }

        if (current_sum <= t && current_sum > best_sum) {
            best_sum = current_sum;
        }
    } while (std::prev_permutation(mask.begin(), mask.end()));

    return best_sum;
}

int main() {
    // Test cases
    std::vector<int> ts = {50, 55, 56, 57, 58};
    std::vector<int> xs = {50};
    std::vector<int> ys = {91, 74, 73, 85, 73, 81, 87};

    std::cout << "Test 1: " << BestTravel::chooseBestSum(163, 3, ts) << std::endl;  // Expected 163
    std::cout << "Test 2: " << BestTravel::chooseBestSum(163, 3, xs) << std::endl;  // Expected -1
    std::cout << "Test 3: " << BestTravel::chooseBestSum(230, 3, ys) << std::endl;  // Expected 288

    return 0;
}