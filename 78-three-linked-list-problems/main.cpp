#include <iostream>
#include <stdexcept>

struct ListNode {
    int val;
    ListNode* next;
    explicit ListNode(int value) : val(value), next(nullptr) {}
};

ListNode* reverseList(ListNode* head) {
    throw std::logic_error("Not implemented");
}

bool hasCycle(ListNode* head) {
    throw std::logic_error("Not implemented");
}

void reorderList(ListNode* head) {
    throw std::logic_error("Not implemented");
}

int main() {
    std::cout << "Implement the linked-list problems for kata 78." << std::endl;
    return 0;
}
