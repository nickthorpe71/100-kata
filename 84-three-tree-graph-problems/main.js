class TreeNode {
  constructor(val, left = null, right = null) {
    this.val = val;
    this.left = left;
    this.right = right;
  }
}

function maxDepth(root) {
  throw new Error("Not implemented");
}

function kthSmallest(root, k) {
  throw new Error("Not implemented");
}

function countIslands(grid) {
  throw new Error("Not implemented");
}

function main() {
  console.log("Implement the three functions for kata 84 and add tests.");
}

if (require.main === module) {
  main();
}

module.exports = {
  TreeNode,
  maxDepth,
  kthSmallest,
  countIslands,
};
