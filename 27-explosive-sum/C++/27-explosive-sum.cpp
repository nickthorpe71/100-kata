#include <iostream>
#include <vector>

using ull = unsigned long long;

ull exp_sum(unsigned int n) {
    std::vector<ull> dp(n + 1, 0); // dp[i] will hold the partition number of i
    dp[0] = 1; // There is one way to partition 0
    
    // Generate and use generalized pentagonal numbers
    for (unsigned int i = 1; i <= n; ++i) {
        for (int k = 1;; k++) {
            int pentagonal1 = k * (3 * k - 1) / 2; // General formula for pentagonal numbers
            int pentagonal2 = k * (3 * k + 1) / 2; // Both positive and negative k
            if (pentagonal1 > n && pentagonal2 > n) break; // Stop if both are out of bounds
            
            // Apply the pentagonal number theorem with sign alternations
            if (pentagonal1 <= n) dp[i] += (k % 2 == 0 ? -dp[i - pentagonal1] : dp[i - pentagonal1]);
            if (pentagonal2 <= n) dp[i] += (k % 2 == 0 ? -dp[i - pentagonal2] : dp[i - pentagonal2]);
        }
    }

    return dp[n];
}

int main() {
    std::cout << "exp_sum(1): " << exp_sum(1) << std::endl;
    std::cout << "exp_sum(2): " << exp_sum(2) << std::endl;
    std::cout << "exp_sum(3): " << exp_sum(3) << std::endl;
    std::cout << "exp_sum(4): " << exp_sum(4) << std::endl;
    std::cout << "exp_sum(5): " << exp_sum(5) << std::endl;
    std::cout << "exp_sum(10): " << exp_sum(10) << std::endl;
    std::cout << "exp_sum(50): " << exp_sum(50) << std::endl;
    std::cout << "exp_sum(80): " << exp_sum(80) << std::endl;
    std::cout << "exp_sum(100): " << exp_sum(100) << std::endl;

    return 0;
}