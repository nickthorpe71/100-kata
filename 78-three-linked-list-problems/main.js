class ListNode {
  constructor(val, next = null) {
    this.val = val;
    this.next = next;
  }
}

function reverseList(head) {
  throw new Error("Not implemented");
}

function hasCycle(head) {
  throw new Error("Not implemented");
}

function reorderList(head) {
  throw new Error("Not implemented");
}

function main() {
  console.log("Implement the linked-list problems for kata 78.");
}

if (require.main === module) {
  main();
}

module.exports = {
  ListNode,
  reverseList,
  hasCycle,
  reorderList,
};
