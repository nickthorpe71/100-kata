#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> gondola_line::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        701, "Count the Cars", WORLD_NAME, WORLD_DESC,
        "Count the number of gondolas in the line. Return the count. head2 is null, target is unused. Return value is cast from the returned ListNode* — return (ListNode*)(long long)count.",
        "Example: 1->2->3 -> 3\nExample: 5 -> 1\nExample: null -> 0",
        "0 <= N <= 100,000",
        100000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Walk the list with a pointer, incrementing a counter each step. When the pointer reaches null, the counter holds the length. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        702, "Inspect Car", WORLD_NAME, WORLD_DESC,
        "Return the value of the gondola at position target (0-indexed). Return (ListNode*)-1 if out of bounds. head2 is null.",
        "Example: 1->2->3, target=1 -> 2\nExample: 1->2->3, target=5 -> -1\nExample: 7, target=0 -> 7",
        "0 <= N <= 100,000\n0 <= target",
        100000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Walk with a pointer and a counter. When the counter equals the target index, return the current node's value. If the list ends first, return -1. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        703, "Reverse the Line", WORLD_NAME, WORLD_DESC,
        "Motor hiccup — reverse the entire line. Return the new head. head2 is null, target is unused.",
        "Example: 1->2->3 -> 3->2->1\nExample: 5 -> 5\nExample: null -> null",
        "0 <= N <= 200,000",
        200000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Three pointers: prev, curr, next. For each node, save next, point curr->next to prev, then advance. Return prev as the new head. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        704, "Find the Midpoint", WORLD_NAME, WORLD_DESC,
        "Find the middle gondola for inspection. Return a pointer to the middle node. For even length, return the second middle. head2 is null, target is unused.",
        "Example: 1->2->3->4->5 -> node with val 3\nExample: 1->2->3->4 -> node with val 3 (second middle)\nExample: 1 -> node with val 1",
        "1 <= N <= 200,000",
        200000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Fast/slow pointers. Slow advances 1 step, fast advances 2. When fast reaches the end, slow is at the middle node. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        705, "Detect the Loop", WORLD_NAME, WORLD_DESC,
        "The track looped back on itself. Return (ListNode*)1 if a cycle exists, (ListNode*)0 otherwise. head2 is null, target is unused.",
        "Example: 1->2->3->2(cycle) -> 1\nExample: 1->2->3 -> 0\nExample: null -> 0",
        "0 <= N <= 200,000",
        200000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Floyd's cycle detection. Fast and slow pointers -- if they meet, there is a cycle. If fast reaches null, there is no cycle. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        706, "Merge Routes", WORLD_NAME, WORLD_DESC,
        "Two sorted gondola lines merging onto one track. Merge head and head2 into a single sorted line. Return the merged head. target is unused.",
        "Example: 1->3->5, 2->4->6 -> 1->2->3->4->5->6\nExample: null, 1->2 -> 1->2\nExample: 1, 1 -> 1->1",
        "0 <= N, M <= 200,000\nBoth sorted ascending.",
        200000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Compare heads of both sorted lists. Append the smaller node to the result and advance that list's pointer. When one list is exhausted, append the other. O(n+m).", generateStub, generateRunner
    });

    waves.push_back({
        707, "Batch Recouple", WORLD_NAME, WORLD_DESC,
        "Reverse every target gondolas for platform compatibility. If the last group has fewer than target nodes, reverse them too. Return the new head. head2 is null.",
        "Example: 1->2->3->4->5, target=2 -> 2->1->4->3->5\nExample: 1->2->3->4, target=3 -> 3->2->1->4\nExample: 1->2->3, target=1 -> 1->2->3",
        "1 <= N <= 200,000\n1 <= target <= N",
        200000, 1000, 300,
        "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target)",
        "Count k nodes, reverse that group using the 3-pointer technique, then connect to the next group. Repeat. Handle the last group specially if it has fewer than k nodes. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a linked-list pointer approach",
        "Need sequential node traversal and careful pointer rewiring without random access",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Pointer-based merge of sorted linked lists",
            "Binary search over node values",
            "Trie over list digits",
            "Union-Find over node indices"
        },
        0,
        "Need sequential node traversal and careful pointer rewiring without random access",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given a linked list, return the number of nodes in the list.";
    waves[0].flavor_text = "A cable mechanic rides a rattling gondola chain through the cavern, counting each hanging car by touch alone. The line sways in darkness, and only his patient crawl reveals how many links remain.";
    waves[1].clear_prompt = "Given a linked list and an index or search condition specified by the wave, return the requested node or value.";
    waves[1].flavor_text = "The mechanic advances car by car along the hanging route, searching for one exact cabin in the dark. There are no shortcuts here, only the next link and the nerve to follow it.";
    waves[2].clear_prompt = "Given a linked list, reverse the list in place and return the new head.";
    waves[2].flavor_text = "A snapped cable has left the gondola line facing the wrong way. The mechanic braces each joint, one after another, and rewires the whole route so the cars can move back toward the station.";
    waves[3].clear_prompt = "Given a linked list, return the middle node or midpoint value as specified by the wave.";
    waves[3].flavor_text = "The mechanic has to find the exact center of the hanging route, the one car that splits the line into equal dread on both sides. He sends one inspection light racing ahead while another keeps honest pace.";
    waves[4].clear_prompt = "Given a linked list, return whether the list contains a cycle.";
    waves[4].flavor_text = "Something is wrong in the cavern. The cars pass overhead again and again, and the mechanic suspects the line bends back into itself. He needs proof before the whole route becomes a trap.";
    waves[5].clear_prompt = "Given two sorted linked lists, merge them into one sorted linked list and return its head.";
    waves[5].flavor_text = "Two gondola routes descend from opposite cliffs, each already ordered by cargo weight. The mechanic must weave them into one clean line without dropping a single car into the abyss.";
    waves[6].clear_prompt = "Given a linked list and the batch size specified by the wave, reverse nodes in consecutive groups and return the new head.";
    waves[6].flavor_text = "The mechanic works down the chain in disciplined bursts, uncoupling and recoupling cars in fixed-size batches. Each block must flip cleanly while the rest of the route waits in tense silence.";
    return waves;
}

void gondola_line::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <cstdlib>\n";
    out << "#include <vector>\n";
    out << "#include <string>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "using namespace std;\n\n";

    out << "struct ListNode {\n";
    out << "    int val;\n";
    out << "    ListNode* next;\n";
    out << "    ListNode(int v) : val(v), next(nullptr) {}\n";
    out << "};\n\n";

    out << "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target);\n\n";

    out << "ListNode* buildList(vector<int>& vals) {\n";
    out << "    if (vals.empty()) return nullptr;\n";
    out << "    ListNode* head = new ListNode(vals[0]);\n";
    out << "    ListNode* curr = head;\n";
    out << "    for (int i = 1; i < (int)vals.size(); i++) {\n";
    out << "        curr->next = new ListNode(vals[i]);\n";
    out << "        curr = curr->next;\n";
    out << "    }\n";
    out << "    return head;\n";
    out << "}\n\n";

    out << "vector<int> listToVec(ListNode* head, int maxLen = 1000000) {\n";
    out << "    vector<int> r;\n";
    out << "    while (head && (int)r.size() < maxLen) {\n";
    out << "        r.push_back(head->val);\n";
    out << "        head = head->next;\n";
    out << "    }\n";
    out << "    return r;\n";
    out << "}\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 701:
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {5};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        long long r = (long long)fixTheLine(nullptr, nullptr, 0);\n";
            out << "        if (r != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n, 1);\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 702:
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 1);\n";
            out << "        if (r != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 5);\n";
            out << "        if (r != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {7};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {10, 20, 30, 40, 50};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 4);\n";
            out << "        if (r != 50) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i + 1;\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, n - 1);\n";
            out << "        if (r != n) all_correct = false;\n";
            out << "        h = buildList(v);\n";
            out << "        r = (long long)fixTheLine(h, nullptr, n);\n";
            out << "        if (r != -1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 703:
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({3, 2, 1})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {5};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({5})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        auto* r = fixTheLine(nullptr, nullptr, 0);\n";
            out << "        if (r != nullptr) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4, 5};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({5, 4, 3, 2, 1})) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i + 1;\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        if (!r || r->val != n) all_correct = false;\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if ((int)rv.size() != n || rv[n - 1] != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 704:
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4, 5};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        if (!r || r->val != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        if (!r || r->val != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        if (!r || r->val != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        if (!r || r->val != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i + 1;\n";
            out << "        auto* h = buildList(v);\n";
            out << "        auto* r = fixTheLine(h, nullptr, 0);\n";
            out << "        int expected_mid = n / 2 + 1;\n";
            out << "        if (!r || r->val != expected_mid) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 705:
            out << "    {\n";
            out << "        auto* n1 = new ListNode(1);\n";
            out << "        auto* n2 = new ListNode(2);\n";
            out << "        auto* n3 = new ListNode(3);\n";
            out << "        n1->next = n2; n2->next = n3; n3->next = n2;\n";
            out << "        long long r = (long long)fixTheLine(n1, nullptr, 0);\n";
            out << "        if (r != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* h = buildList(v);\n";
            out << "        long long r = (long long)fixTheLine(h, nullptr, 0);\n";
            out << "        if (r != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        long long r = (long long)fixTheLine(nullptr, nullptr, 0);\n";
            out << "        if (r != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        auto* n1 = new ListNode(1);\n";
            out << "        n1->next = n1;\n";
            out << "        long long r = (long long)fixTheLine(n1, nullptr, 0);\n";
            out << "        if (r != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<ListNode*> nodes(n);\n";
            out << "        for (int i = 0; i < n; i++) nodes[i] = new ListNode(i);\n";
            out << "        for (int i = 0; i < n - 1; i++) nodes[i]->next = nodes[i + 1];\n";
            out << "        nodes[n - 1]->next = nodes[n / 2];\n";
            out << "        long long r = (long long)fixTheLine(nodes[0], nullptr, 0);\n";
            out << "        if (r != 1) all_correct = false;\n";
            out << "        for (int i = 0; i < n; i++) nodes[i] = new ListNode(i);\n";
            out << "        for (int i = 0; i < n - 1; i++) nodes[i]->next = nodes[i + 1];\n";
            out << "        r = (long long)fixTheLine(nodes[0], nullptr, 0);\n";
            out << "        if (r != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 706:
            out << "    {\n";
            out << "        vector<int> v1 = {1, 3, 5}, v2 = {2, 4, 6};\n";
            out << "        auto* r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1, 2, 3, 4, 5, 6})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v1 = {}, v2 = {1, 2};\n";
            out << "        auto* r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1, 2})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v1 = {1}, v2 = {1};\n";
            out << "        auto* r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1, 1})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v1 = {1, 1, 1}, v2 = {2, 2, 2};\n";
            out << "        auto* r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1, 1, 1, 2, 2, 2})) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v1(n), v2(n);\n";
            out << "        for (int i = 0; i < n; i++) { v1[i] = i * 2; v2[i] = i * 2 + 1; }\n";
            out << "        auto* r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if ((int)rv.size() != 2 * n || rv[0] != 0 || rv[1] != 1) all_correct = false;\n";
            out << "        ops = 2 * n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 707:
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4, 5};\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 2);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({2, 1, 4, 3, 5})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4};\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 3);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({3, 2, 1, 4})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 1);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1, 2, 3})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4, 5, 6};\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 3);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({3, 2, 1, 6, 5, 4})) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 1);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if (rv != vector<int>({1})) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i + 1;\n";
            out << "        auto* r = fixTheLine(buildList(v), nullptr, 2);\n";
            out << "        auto rv = listToVec(r);\n";
            out << "        if ((int)rv.size() != n) all_correct = false;\n";
            out << "        if (rv[0] != 2 || rv[1] != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;
    }

    out << "\n    auto end = chrono::high_resolution_clock::now();\n";
    out << "    int ms = (int)chrono::duration_cast<chrono::milliseconds>(end - start).count();\n\n";
    out << "    printf(\"%d %d\\n\", ms, ops);\n";
    out << "    return all_correct ? 0 : 1;\n";
    out << "}\n";

    out.close();
  } else {
    std::string path = player_dir + "/runner.js";
    std::ofstream out(path);

    out << "const { fixTheLine, ListNode } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n\n";

    // JS helper functions
    out << "function buildList(arr) {\n";
    out << "    if (!arr.length) return null;\n";
    out << "    let head = new ListNode(arr[0]);\n";
    out << "    let curr = head;\n";
    out << "    for (let i = 1; i < arr.length; i++) {\n";
    out << "        curr.next = new ListNode(arr[i]);\n";
    out << "        curr = curr.next;\n";
    out << "    }\n";
    out << "    return head;\n";
    out << "}\n\n";

    out << "function listToVec(head, maxLen = 1000000) {\n";
    out << "    let r = [];\n";
    out << "    while (head && r.length < maxLen) {\n";
    out << "        r.push(head.val);\n";
    out << "        head = head.next;\n";
    out << "    }\n";
    out << "    return r;\n";
    out << "}\n\n";

    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 701: // Count the Cars -- returns integer directly in JS
            out << "if (fixTheLine(buildList([1, 2, 3]), null, 0) !== 3) allCorrect = false;\n";
            out << "if (fixTheLine(buildList([5]), null, 0) !== 1) allCorrect = false;\n";
            out << "if (fixTheLine(null, null, 0) !== 0) allCorrect = false;\n";
            out << "if (fixTheLine(buildList([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]), null, 0) !== 10) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v = new Array(n).fill(1);\n";
            out << "    if (fixTheLine(buildList(v), null, 0) !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 702: // Inspect Car -- returns integer directly in JS
            out << "if (fixTheLine(buildList([1, 2, 3]), null, 1) !== 2) allCorrect = false;\n";
            out << "if (fixTheLine(buildList([1, 2, 3]), null, 5) !== -1) allCorrect = false;\n";
            out << "if (fixTheLine(buildList([7]), null, 0) !== 7) allCorrect = false;\n";
            out << "if (fixTheLine(buildList([10, 20, 30, 40, 50]), null, 4) !== 50) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v = [];\n";
            out << "    for (let i = 0; i < n; i++) v.push(i + 1);\n";
            out << "    if (fixTheLine(buildList(v), null, n - 1) !== n) allCorrect = false;\n";
            out << "    if (fixTheLine(buildList(v), null, n) !== -1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 703: // Reverse the Line -- returns ListNode in JS
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3]), null, 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([3, 2, 1])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([5]), null, 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([5])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(null, null, 0);\n";
            out << "    if (r !== null) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4, 5]), null, 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([5, 4, 3, 2, 1])) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v = [];\n";
            out << "    for (let i = 0; i < n; i++) v.push(i + 1);\n";
            out << "    let r = fixTheLine(buildList(v), null, 0);\n";
            out << "    if (!r || r.val !== n) allCorrect = false;\n";
            out << "    let rv = listToVec(r);\n";
            out << "    if (rv.length !== n || rv[n - 1] !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 704: // Find the Midpoint -- returns ListNode in JS
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4, 5]), null, 0);\n";
            out << "    if (!r || r.val !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4]), null, 0);\n";
            out << "    if (!r || r.val !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1]), null, 0);\n";
            out << "    if (!r || r.val !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2]), null, 0);\n";
            out << "    if (!r || r.val !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v = [];\n";
            out << "    for (let i = 0; i < n; i++) v.push(i + 1);\n";
            out << "    let r = fixTheLine(buildList(v), null, 0);\n";
            out << "    let expectedMid = Math.floor(n / 2) + 1;\n";
            out << "    if (!r || r.val !== expectedMid) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 705: // Detect the Loop -- returns integer in JS
            out << "{\n";
            out << "    let n1 = new ListNode(1);\n";
            out << "    let n2 = new ListNode(2);\n";
            out << "    let n3 = new ListNode(3);\n";
            out << "    n1.next = n2; n2.next = n3; n3.next = n2;\n";
            out << "    if (fixTheLine(n1, null, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (fixTheLine(buildList([1, 2, 3]), null, 0) !== 0) allCorrect = false;\n";
            out << "if (fixTheLine(null, null, 0) !== 0) allCorrect = false;\n";
            out << "{\n";
            out << "    let n1 = new ListNode(1);\n";
            out << "    n1.next = n1;\n";
            out << "    if (fixTheLine(n1, null, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let nodes = [];\n";
            out << "    for (let i = 0; i < n; i++) nodes.push(new ListNode(i));\n";
            out << "    for (let i = 0; i < n - 1; i++) nodes[i].next = nodes[i + 1];\n";
            out << "    nodes[n - 1].next = nodes[Math.floor(n / 2)];\n";
            out << "    if (fixTheLine(nodes[0], null, 0) !== 1) allCorrect = false;\n";
            out << "    nodes = [];\n";
            out << "    for (let i = 0; i < n; i++) nodes.push(new ListNode(i));\n";
            out << "    for (let i = 0; i < n - 1; i++) nodes[i].next = nodes[i + 1];\n";
            out << "    if (fixTheLine(nodes[0], null, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 706: // Merge Routes -- returns ListNode in JS
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 3, 5]), buildList([2, 4, 6]), 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1, 2, 3, 4, 5, 6])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([]), buildList([1, 2]), 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1, 2])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1]), buildList([1]), 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1, 1])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 1, 1]), buildList([2, 2, 2]), 0);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1, 1, 1, 2, 2, 2])) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v1 = [], v2 = [];\n";
            out << "    for (let i = 0; i < n; i++) { v1.push(i * 2); v2.push(i * 2 + 1); }\n";
            out << "    let r = fixTheLine(buildList(v1), buildList(v2), 0);\n";
            out << "    let rv = listToVec(r);\n";
            out << "    if (rv.length !== 2 * n || rv[0] !== 0 || rv[1] !== 1) allCorrect = false;\n";
            out << "    ops = 2 * n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 707: // Batch Recouple (boss) -- returns ListNode in JS
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4, 5]), null, 2);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([2, 1, 4, 3, 5])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4]), null, 3);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([3, 2, 1, 4])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3]), null, 1);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1, 2, 3])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1, 2, 3, 4, 5, 6]), null, 3);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([3, 2, 1, 6, 5, 4])) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = fixTheLine(buildList([1]), null, 1);\n";
            out << "    if (JSON.stringify(listToVec(r)) !== JSON.stringify([1])) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let v = [];\n";
            out << "    for (let i = 0; i < n; i++) v.push(i + 1);\n";
            out << "    let r = fixTheLine(buildList(v), null, 2);\n";
            out << "    let rv = listToVec(r);\n";
            out << "    if (rv.length !== n) allCorrect = false;\n";
            out << "    if (rv[0] !== 2 || rv[1] !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
