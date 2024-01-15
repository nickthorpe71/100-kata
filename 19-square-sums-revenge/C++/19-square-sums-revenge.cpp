#include <vector>
#include <cmath>
#include <unordered_set>
#include <algorithm>
#include <numeric>
#include <functional>
#include <unordered_map>

std::vector<int> square_sums_row(int n) {
    std::unordered_set<int> squares;
    for (int i = 1; i * i <= 2 * n; ++i) {
        squares.insert(i * i);
    }

    std::vector<int> path;
    std::vector<bool> used(n + 1, false);
    std::unordered_map<int, std::vector<int>> memo;

    std::function<bool(int)> dfs;
    dfs = [&](int v) -> bool {
        if (v == n + 1) return true;

        int hash = 0;
        for (int i = 1; i <= n; ++i) {
            if (!used[i] && (path.empty() || squares.count(path.back() + i))) {
                path.push_back(i);
                used[i] = true;
                hash ^= (1 << i);

                if (dfs(v + 1)) return true;

                path.pop_back();
                used[i] = false;
                hash ^= (1 << i);
            }
        }

        if (memo.count(hash) == 0) {
            memo[hash] = path;
        }

        return false;
    };

    dfs(1);

    return path;
}

// TESTS / MAIN

#include <iostream>
#include <set>

bool is_valid_solution(const std::vector<int>& solution) {
    if (solution.empty()) {
        return false; // No solution
    }
    
    std::set<int> used_numbers;
    for (int i = 0; i < solution.size(); ++i) {
        if (used_numbers.find(solution[i]) != used_numbers.end()) {
            return false; // Duplicate number
        }
        used_numbers.insert(solution[i]);
        if (i > 0) {
            int sum = solution[i] + solution[i - 1];
            int root = std::sqrt(sum);
            if (root * root != sum) {
                return false; // Sum is not a perfect square
            }
        }
    }
    return true;
}

void print_solution(const std::vector<int>& solution) {
    if (solution.empty()) {
        std::cout << "No solution found." << std::endl;
        return;
    }
    for (int num : solution) {
        std::cout << num << " ";
    }
    std::cout << std::endl;
}

void run_test(int n) {
    std::cout << "Testing N = " << n << std::endl;
    auto solution = square_sums_row(n);
    if (is_valid_solution(solution)) {
        std::cout << "Valid solution found: ";
        print_solution(solution);
    } else {
        std::cout << "Invalid solution or no solution." << std::endl;
    }
}

int main() {
    // Test cases
    run_test(15); // Should find a solution
    run_test(23); // Might or might not find a solution
    run_test(5);  // Should not find a solution

    // You can add more tests here
    return 0;
}