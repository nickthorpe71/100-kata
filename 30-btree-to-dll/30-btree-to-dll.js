/**
 * @typedef {object} Node
 * @property {number} value
 * @property {Node[]} children
 */

/**
 * @typedef {object} DLLNode
 * @property {DLLNode} next
 * @property {DLLNode} prev
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
 * @param {number} value
 * @param {DLLNode} next 
 * @param {DLLNode} prev 
 * @returns {DLLNode}
 */
function createDLLNode(value, next = null, prev = null) {
    return ({
        value,
        next,
        prev
    })
}

/**
 * @param {Node} node 
 * @param {(value: number) => void} onVisit 
 */
function inOrderTraversal(node, onVisit) {
    if (node.children[0]) {
        inOrderTraversal(node.children[0], onVisit);
    }
    onVisit(node.value);
    if (node.children[1]) {
        inOrderTraversal(node.children[1], onVisit);
    }
}

/**
 * @param {Node} root 
 * @returns {DLLNode}
 */
function convertBTreeToDLL(root) {
    let head = createDLLNode(null);
    let current = head;
    inOrderTraversal(root, (value) => {
        const newNode = createDLLNode(value);
        newNode.prev = current;
        current.next = newNode;
        current = newNode;
    });
    return head;
}

function printDLL(node) {
    let current = node.next;
    while (current !== null) {
        console.log(current.value);
        current = current.next;
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
    
    const resDll = convertBTreeToDLL(exampleBTree);
    printDLL(resDll);
}

main();