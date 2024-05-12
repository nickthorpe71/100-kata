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
 * @param {Node} graph
 * @param {function(Node): void} onVisit
 * @returns {void}
 */
function bfs(graph, onVisit) {
  const q = [graph];

  while (q.length > 0) {
    const current = q.shift();
    onVisit(current);
    current.children.forEach(child => q.push(child));
  }
}

/**
 * @param {Node} graph 
 * @param {function(Node): void} onVisit 
 */
function dfs(graph, onVisit) {
  onVisit(graph);
  graph.children.forEach(child => rbfs(child, onVisit));
}

function main() {
  const graph = createNode(1, [createNode(2, [createNode(4), createNode(5)]), createNode(3)]);
  rbfs(graph, (node) => console.log(node.value)); 
}

main();