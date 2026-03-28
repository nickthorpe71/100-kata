#include <iostream>
#include <stdexcept>
#include <vector>
#include <algorithm>

using namespace std;

struct TreeNode {
    int val;
    TreeNode* left;
    TreeNode* right;
    explicit TreeNode(int value) : val(value), left(nullptr), right(nullptr) {}
};

int maxDepth(TreeNode* root) {
    if (root == nullptr) {
        return 0;
    }
    
    return std::max(maxDepth(root->left), maxDepth(root->right)) + 1; 
}

int kthSmallest(TreeNode* root, int k) {
    throw std::logic_error("Not implemented");
}

void markIsland(int row, int col, vector<vector<char>>& grid);
bool outOfBounds(int row, int col, const vector<vector<char>>& grid);

int countIslands(std::vector<std::vector<char>>& grid) {
    if (grid.size() == 0 || grid[0].size() == 0) {
        return 0; 
    }

    int total = 0;
    for (int i = 0; i < grid.size(); i++) {
        for (int j = 0; j < grid[0].size(); j++) {
            if (grid[i][j] == '0') {
                continue;
            }
            total++;
            markIsland(i,j,grid);
        }
    }
    return total;
}

void markIsland(int row, int col, vector<vector<char>>& grid) {
    if (outOfBounds(row, col, grid) || grid[row][col] == '0') {
        return;
    }

    grid[row][col] = '0';

    markIsland(row + 1, col, grid);
    markIsland(row, col + 1, grid);
    markIsland(row - 1, col, grid);
    markIsland(row, col - 1, grid);

    return;
}

bool outOfBounds(int row, int col, const vector<vector<char>>& grid) {
    return row < 0 
        || col < 0
        || row >= grid.size()
        || col >= grid[0].size();
}

int main() {
    std::cout << "Implement the three functions for kata 84 and add tests." << std::endl;
    return 0;
}
