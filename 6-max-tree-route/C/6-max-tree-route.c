#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include<math.h>
#include<limits.h>

// Definition of the TreeNode struct
typedef struct TreeNode {
  int value;
  struct TreeNode* left;
  struct TreeNode* right;
} TreeNode;

// Function to create a new TreeNode
TreeNode* newTreeNode(int value, TreeNode* left, TreeNode* right) {
  TreeNode* node = (TreeNode*)malloc(sizeof(TreeNode));
  if (!node) {
    return NULL;
  }
  node->value = value;
  node->left = left;
  node->right = right;
  return node;
}

// Function to calculate the maximum sum from root to any leaf
int maxSum(TreeNode* root) {
  if (root == NULL) {
    return 0;
  } else if (root->left == NULL && root->right == NULL) {
    return root->value;
  } else {
    int leftMax = root->left ? maxSum(root->left) : INT_MIN;
    int rightMax = root-> right ? maxSum(root->right) : INT_MIN;
    return root->value + (leftMax > rightMax ? leftMax : rightMax);
  }
}

// Function to free the memory of a tree
void freeTree(TreeNode* root) {
  if (root == NULL) return;
  freeTree(root->left);
  freeTree(root->right);
  free(root);
}

int main() 
{
  clock_t start, end;
  double cpu_time_used;

  start = clock();
  
  // Create sample tree
  TreeNode* sampleTree = newTreeNode(
    17,
    newTreeNode(
      3,
      newTreeNode(2, NULL, NULL),
      NULL
    ),
    newTreeNode(
      -10,
      newTreeNode(16, NULL, NULL),
      newTreeNode(
        1,
        newTreeNode(13, NULL, NULL),
        NULL
      )
    )
  );

  // Calculate the max sum of any one tree path
  printf("Max Sum: %d\n", maxSum(sampleTree));

  freeTree(sampleTree);

  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
  printf("Time elapsed: %f\n", cpu_time_used);
  return 0;
}

