#include <iostream>
#include <unordered_set>

class Node
{
public:
    Node *next;
    Node() : next(nullptr) {}
};

int getLengthOfCycle(Node *startNode)
{
    std::unordered_set<Node *> visited;
    Node *current = startNode;

    while (current != nullptr)
    {
        // If the current node is already visited, we've found the loop
        if (visited.find(current) != visited.end())
        {
            // Start measuring the loop size
            Node *loopNode = current;
            int loopSize = 0;
            do
            {
                current = current->next;
                loopSize++;
            } while (current != loopNode);

            return loopSize;
        }

        // Otherwise, add the current node to the visited set and move to the next node
        visited.insert(current);
        current = current->next;
    }

    // If the loop is not found, return 0
    return 0;
}

int tortoiseHare(Node *startNode)
{
    Node *tortoise = startNode;
    Node *hare = startNode->next;

    // Move the hare two nodes ahead of the tortoise
    while (hare != nullptr && hare->next != nullptr)
    {
        tortoise = tortoise->next;
        hare = hare->next->next;

        // If the tortoise and hare meet, there is a loop
        if (tortoise == hare)
        {
            // Start measuring the loop size
            int loopSize = 0;
            do
            {
                tortoise = tortoise->next;
                loopSize++;
            } while (tortoise != hare);

            return loopSize;
        }
    }

    // If the loop is not found, return 0
    return 0;
}

int main()
{
    // Example usage
    Node *head = new Node();
    Node *second = new Node();
    Node *third = new Node();
    Node *fourth = new Node();
    Node *fifth = new Node();

    head->next = second;
    second->next = third;
    third->next = fourth;
    fourth->next = fifth;
    fifth->next = third; // Creating a loop back to the third node

    std::cout << "Loop size: " << getLengthOfCycle(head) << std::endl;

    // Clean up (would ideally use smart pointers or a destructor in a real-world scenario)
    delete fifth;
    delete fourth;
    delete third;
    delete second;
    delete head;

    return 0;
}