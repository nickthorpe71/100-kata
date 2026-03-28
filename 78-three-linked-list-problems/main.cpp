#include <iostream>
#include <stdexcept>

struct ListNode {
    int val;
    ListNode* next;
    explicit ListNode(int value) : val(value), next(nullptr) {}
};

ListNode* reverseList(ListNode* head) {
    ListNode* prev = nullptr;
    ListNode* curr = head;
    ListNode* next = nullptr;

    while (curr != nullptr) {
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }

    return prev;
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
