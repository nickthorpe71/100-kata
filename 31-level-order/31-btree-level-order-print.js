/**
 * @typedef {object} Node
 * @property {number} value
 * @property {Node[]} children
 */

/**
 * @param {number} value
 * @param {Node[]} children
 * @returns {Node}
 */
function createNode(value, children = []) {
  return ({
    value,
    children,
  });
}

/**
 * @param {Node} node
 * @param {(value: number) => void} onVisit
 * @returns {void}
 */
function levelOrderTraversal(node, onVisit) {
    const q = [node];
    
    while (q.length > 0) {
        const curr = q.shift();
        for (const child of curr.children) {
            q.push(child);
        }
        onVisit(curr.value);
    }
}

function main() {
    const exampleBTree = createNode(17, [
        createNode(22, [
            createNode(95), createNode(30)
        ]),
        createNode(25, [
            createNode(66)
        ])
    ]);
    
    levelOrderTraversal(exampleBTree, (value) => console.log(value));
}

main();