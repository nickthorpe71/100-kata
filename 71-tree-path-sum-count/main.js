/*
  HackerRank-style starter:
  Complete the `countTargetPaths` function below.

  Return the number of root-to-leaf paths whose values add up to targetSum.
*/

function countTargetPaths(root, targetSum) {
    return processNode(root, 0, targetSum);
}

function processNode(node, totalSoFar, targetSum) {
    if (node === null) {
        return 0;
    }
    const newTotal = totalSoFar + node.value;
    const nodeIsLeaf = node.left === null && node.right === null;
    if (!nodeIsLeaf) {
        return processNode(node.left, newTotal, targetSum) + processNode(node.right, newTotal, targetSum);
    }

    // we know the node is a leaf so we compare to the target
    return (newTotal === targetSum) ? 1 : 0;
}

function node(value, left = null, right = null) {
    return { value, left, right };
}










// ------------------------------------------

function runTests() {
    const treeA = node(
        5,
        node(4, node(11, node(7), node(2))),
        node(8, node(13), node(4, null, node(1)))
    );

    const treeB = node(
        1,
        node(2, node(3), node(4)),
        node(5, null, node(1))
    );

    const tests = [
        { root: treeA, targetSum: 22, expected: 1 },
        { root: treeB, targetSum: 6, expected: 1 },
        { root: treeB, targetSum: 8, expected: 1 },
        { root: null, targetSum: 10, expected: 0 },
    ];

    for (const { root, targetSum, expected } of tests) {
        const actual = countTargetPaths(root, targetSum);
        console.log({
            targetSum,
            expected,
            actual,
            passed: actual === expected,
        });
    }
}

runTests();
