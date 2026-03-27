#include <iostream>
#include <stdexcept>
#include <vector>

struct ListNode {
    int val;
    ListNode* next;
    explicit ListNode(int value) : val(value), next(nullptr) {}
};

ListNode* mergeKLists(std::vector<ListNode*>& lists) {
    throw std::logic_error("Not implemented");
}

int main() {
    std::cout << "Implement mergeKLists for kata 91 and add linked-list tests." << std::endl;
    return 0;
}
