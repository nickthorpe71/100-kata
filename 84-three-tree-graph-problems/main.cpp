#include <iostream>
#include <stdexcept>
#include <vector>

struct TreeNode {
    int val;
    TreeNode* left;
    TreeNode* right;
    explicit TreeNode(int value) : val(value), left(nullptr), right(nullptr) {}
};

int maxDepth(TreeNode* root) {
    throw std::logic_error("Not implemented");
}

int kthSmallest(TreeNode* root, int k) {
    throw std::logic_error("Not implemented");
}

int countIslands(std::vector<std::vector<char>>& grid) {
    throw std::logic_error("Not implemented");
}

int main() {
    std::cout << "Implement the three functions for kata 84 and add tests." << std::endl;
    return 0;
}
