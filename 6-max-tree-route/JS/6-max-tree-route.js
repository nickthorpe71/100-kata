class TreeNode {
    constructor(value, left = null, right = null) {
        this.value = value;
        this.left = left;
        this.right = right;
    }
}

function stringifyTree(root) {
    return root.value === null
        ? ""
        : `${root.value}${root.left ? "\n" + stringifyTree(root.left) : ""}${
              root.right ? "\n" + stringifyTree(root.right) : ""
          }`;
}

function maxSumOld(root) {
    if (root === null) {
        return 0;
    }

    const leftMax = maxSum(root.left);
    const rightMax = maxSum(root.right);

    return Math.max(leftMax, rightMax) + root.value;
}

const maxSum = (root) =>
    root === null
        ? 0
        : root.left === null && root.right === null
        ? root.value
        : root.value +
          Math.max(
              root.left ? maxSum(root.left) : -Infinity,
              root.right ? maxSum(root.right) : -Infinity
          );

function main() {
    console.time("Execution Time");
    const sampleTree = new TreeNode(
        17,
        new TreeNode(3, new TreeNode(2)),
        new TreeNode(-10, new TreeNode(16), new TreeNode(1, new TreeNode(13)))
    );
    console.log(maxSum(sampleTree));
    console.timeEnd("Execution Time");
}

main();
