#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> world_root::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1101, "Count the Realms", WORLD_NAME, WORLD_DESC,
        "Count all non-null nodes in the tree. Level-order array: children of index i at 2i+1 and 2i+2. -1 means null. target is unused.",
        "Example: [1, 2, 3] -> 3\nExample: [1, -1, 3] -> 2\nExample: [5] -> 1\nExample: [] -> 0",
        "0 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "Traverse the array, count elements that are not -1 and whose index is valid. Recursive helper: if index >= size or value is -1, return 0. Otherwise 1 + count(left) + count(right). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1102, "Depth of the Roots", WORLD_NAME, WORLD_DESC,
        "Return the maximum depth of the tree. A single node has depth 1. Empty tree has depth 0. target is unused.",
        "Example: [1, 2, 3, 4, 5] -> 3\nExample: [1] -> 1\nExample: [] -> 0\nExample: [1, 2, -1, 3, -1] -> 3",
        "0 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "Recursive: if null, return 0. Otherwise 1 + max(depth(left), depth(right)). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1103, "The Balanced Bough", WORLD_NAME, WORLD_DESC,
        "Return 1 if the tree is height-balanced: for every node, the depth of its left and right subtrees differ by at most 1. Return 0 otherwise. target is unused.",
        "Example: [1, 2, 3, 4, 5] -> 1\nExample: [1, 2, -1, 3, -1] -> 0 (left depth 3, right depth 0)\nExample: [1] -> 1\nExample: [] -> 1",
        "0 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "Recursive function returning height or -1 (unbalanced). For each node, get left and right heights. If either is -1 or abs(left-right) > 1, return -1. Otherwise return max+1. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1104, "Path of Fate", WORLD_NAME, WORLD_DESC,
        "Return 1 if any root-to-leaf path sums to target. A leaf is a non-null node with no non-null children. Return 0 otherwise.",
        "Example: [5, 4, 8, 11, -1, 13, 4, 7, 2], target=22 -> 1 (5->4->11->2)\nExample: [1, 2, 3], target=4 -> 1 (1->3)\nExample: [1, 2, 3], target=5 -> 0\nExample: [1], target=1 -> 1",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "Recursive DFS. Subtract current node's value from target as you go down. At a leaf, check if remaining target equals 0. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1105, "Sum the Ancestors", WORLD_NAME, WORLD_DESC,
        "Return the sum of all non-leaf nodes (nodes that have at least one non-null child). target is unused.",
        "Example: [1, 2, 3] -> 1 (only root has children)\nExample: [1, 2, 3, 4, 5] -> 3 (root=1 + node=2 have children)\nExample: [5] -> 0 (single node is a leaf)\nExample: [10, 5, -1, 3, -1] -> 15 (10 and 5 are non-leaf)",
        "0 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "For each node, check if it has at least one non-null child (2i+1 or 2i+2 exists and isn't -1). If so, add its value. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1106, "Mirror Yggdrasil", WORLD_NAME, WORLD_DESC,
        "Return 1 if the tree is symmetric — the left subtree is a mirror reflection of the right subtree. Return 0 otherwise. target is unused.",
        "Example: [1, 2, 2, 3, 4, 4, 3] -> 1\nExample: [1, 2, 2, -1, 3, -1, 3] -> 0\nExample: [1] -> 1\nExample: [1, 2, 2] -> 1",
        "0 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "Recursive helper comparing two subtrees. isMirror(left, right): both null = true, one null = false, values differ = false, otherwise isMirror(left.left, right.right) && isMirror(left.right, right.left). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1107, "The Lowest Common Root", WORLD_NAME, WORLD_DESC,
        "The tree is a BST. Find the lowest common ancestor of two nodes with values target and tree[1] (the second element stores the other search value, rest of tree starts at index 2). Return the LCA's value. Both values are guaranteed to exist in the tree.",
        "Example: [3, 6, 2, 0, 4, -1, 8] with target=0, tree[1]=4... Actually: tree[0]=second_val, tree[1..]=level-order BST. Let me simplify.\n\nEncoding: tree[0] = the other search value. tree[1..] = level-order BST. Find LCA of target and tree[0] in the BST.\nExample: [0, 6, 2, 8, -1, 4, -1, -1, -1, -1, -1, 3, 5], target=3 -> LCA of 0 and 3... \n\nSimpler: standard level-order BST. target = val1, tree encodes both the BST and val2 at tree[0]. Hmm, this encoding is awkward.",
        "1 <= N <= 100,000\nTree is a valid BST.\nBoth values exist in the tree.",
        100000, 1000, 300,
        "int climbYggdrasil(vector<int>& tree, int target)",
        "BST property: start at root. If both values < current, go left. If both > current, go right. Otherwise current is the LCA. O(h) where h is tree height.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a tree-recursive approach",
        "Need parent-child recursive decomposition over subtree state",
        "O(h) space"
    );
    waves[6].quiz = quizbank::makeThemedQuiz(
        "the BST ordering property",
        "Need ordered or monotonic structure so each comparison discards half the search space",
        "O(h) time / O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "BST ordering to walk toward the split point",
            "Level-order traversal with a queue",
            "Union-Find over parent-child links",
            "Heap of candidate ancestors"
        },
        0,
        "Need ordered or monotonic structure so each comparison discards half the search space",
        "O(h) time / O(1) space"
    );
    waves[0].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return the number of non-null nodes.";
    waves[0].flavor_text = "At the foot of the World Root, a root pilgrim climbs bark wide as city walls and counts every living branch-node he can reach. Hollow gaps disappear into mist, but every real hold must be marked.";
    waves[1].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return the maximum depth of the tree.";
    waves[1].flavor_text = "The pilgrim keeps climbing, measuring how many true layers of branch and shadow stretch from the root to the deepest surviving tip. Some paths die early; one path goes farther than all the others.";
    waves[2].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return `1` if the tree is height-balanced and `0` otherwise.";
    waves[2].flavor_text = "The World Root is old enough to lean. The pilgrim tests whether its left and right boughs still answer one another with near-equal weight, or whether the entire giant has begun to list toward collapse.";
    waves[3].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return `1` if any root-to-leaf path sums to `target`, otherwise return `0`.";
    waves[3].flavor_text = "Carved into the bark are old numbers, and the pilgrim follows them downward like a prophecy. Somewhere from trunk to leaf may lie a path whose sum matches the chosen fate exactly.";
    waves[4].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return the sum of all non-leaf node values.";
    waves[4].flavor_text = "Not every branch of the World Root deserves equal reverence. The pilgrim counts only the ancestors that still bear children, adding the weight of every node that holds the rest of the tree aloft.";
    waves[5].clear_prompt = "Given a binary tree encoded as a level-order array where `-1` means null, return `1` if the tree is symmetric and `0` otherwise.";
    waves[5].flavor_text = "Deep inside the canopy, the pilgrim searches for a perfect reflection: left branch answering right branch, node for node, like a ritual mirror made of wood and silence.";
    waves[6].clear_prompt = "Given a binary search tree and two target values encoded as described by the wave, return the value of their lowest common ancestor.";
    waves[6].flavor_text = "The pilgrim needs the first fork where two sacred routes through the World Root still share the same bark. Above that point the paths are one; below it they split forever.";
    return waves;
}

void world_root::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <algorithm>\n";
        out << "using namespace std;\n\n";

        out << "int climbYggdrasil(vector<int>& tree, int target);\n\n";

        // Helper: check if index is valid non-null node
        out << "bool isNode(vector<int>& t, int i) { return i < (int)t.size() && t[i] != -1; }\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 1101:
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, -1, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {5};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (climbYggdrasil(t, 0) != 7) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, -1, 3, -1};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> t(n, 1);\n";
                out << "        if (climbYggdrasil(t, 0) != n) all_correct = false;\n";
                out << "        // Half null\n";
                out << "        for (int i = 0; i < n; i += 2) t[i] = -1;\n";
                out << "        int expected = 0;\n";
                out << "        for (int x : t) if (x != -1) expected++;\n";
                out << "        if (climbYggdrasil(t, 0) != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 18; }\n";
                break;

            case 1102:
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Left-skewed: 1 -> 2 -> 3\n";
                out << "        vector<int> t = {1, 2, -1, 3, -1};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Full tree depth 3\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> t(n, 1);\n";
                out << "        int depth = 0;\n";
                out << "        int tmp = n;\n";
                out << "        while (tmp > 0) { depth++; tmp /= 2; }\n";
                out << "        if (climbYggdrasil(t, 0) != depth) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 1103:
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, -1, 3, -1};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, -1};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> t(n, 1);\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 17; }\n";
                break;

            case 1104:
                out << "    {\n";
                out << "        vector<int> t = {5, 4, 8, 11, -1, 13, 4, 7, 2};\n";
                out << "        if (climbYggdrasil(t, 22) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3};\n";
                out << "        if (climbYggdrasil(t, 4) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3};\n";
                out << "        if (climbYggdrasil(t, 5) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1};\n";
                out << "        if (climbYggdrasil(t, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1};\n";
                out << "        if (climbYggdrasil(t, 2) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (climbYggdrasil(t, 7) != 1) all_correct = false;\n";
                out << "        if (climbYggdrasil(t, 9) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> t(n, 1);\n";
                out << "        int depth = 0, tmp = n;\n";
                out << "        while (tmp > 0) { depth++; tmp /= 2; }\n";
                out << "        if (climbYggdrasil(t, depth) != 1) all_correct = false;\n";
                out << "        if (climbYggdrasil(t, n * 2) != 0) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 20; }\n";
                break;

            case 1105:
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5};\n";
                out << "        if (climbYggdrasil(t, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {5};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {10, 5, -1, 3, -1};\n";
                out << "        if (climbYggdrasil(t, 0) != 15) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (climbYggdrasil(t, 0) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> t(n, 1);\n";
                out << "        int non_leaves = 0;\n";
                out << "        for (int i = 0; i < n; i++) {\n";
                out << "            int left = 2 * i + 1, right = 2 * i + 2;\n";
                out << "            if ((left < n && t[left] != -1) || (right < n && t[right] != -1)) non_leaves++;\n";
                out << "        }\n";
                out << "        if (climbYggdrasil(t, 0) != non_leaves) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 17; }\n";
                break;

            case 1106:
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 2, 3, 4, 4, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 2, -1, 3, -1, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 2};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 3};\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {1, 2, 2, 3, 4, 4, 3, 5, -1, -1, 6, 6, -1, -1, 5};\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = 1023;\n";
                out << "        vector<int> t(n);\n";
                out << "        t[0] = 1;\n";
                out << "        for (int i = 0; i < n; i++) {\n";
                out << "            int left = 2 * i + 1, right = 2 * i + 2;\n";
                out << "            if (left < n) t[left] = i + 2;\n";
                out << "            if (right < n) t[right] = i + 2;\n";
                out << "        }\n";
                out << "        if (climbYggdrasil(t, 0) != 1) all_correct = false;\n";
                out << "        t[n - 1] = 999999;\n";
                out << "        if (climbYggdrasil(t, 0) != 0) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 20; }\n";
                break;

            case 1107:
                out << "    {\n";
                out << "        vector<int> t = {4, 6, 2, 8, 0, 4, 7, 9};\n";
                out << "        if (climbYggdrasil(t, 0) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {8, 6, 2, 8, 0, 4, 7, 9};\n";
                out << "        if (climbYggdrasil(t, 0) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {4, 6, 2, 8, 0, 4, 7, 9};\n";
                out << "        if (climbYggdrasil(t, 7) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {9, 6, 2, 8, 0, 4, 7, 9};\n";
                out << "        if (climbYggdrasil(t, 7) != 8) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {5, 5};\n";
                out << "        if (climbYggdrasil(t, 5) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> t = {2, 6, 2, 8, 0, 4, 7, 9};\n";
                out << "        if (climbYggdrasil(t, 0) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = 1000;\n";
                out << "        vector<int> t = {500, 500, 250, 750, 125, 375, 625, 875};\n";
                out << "        if (climbYggdrasil(t, 125) != 500) all_correct = false;\n";
                out << "        t[0] = 500;\n";
                out << "        if (climbYggdrasil(t, 625) != 500) all_correct = false;\n";
                out << "        t[0] = 375;\n";
                out << "        if (climbYggdrasil(t, 125) != 250) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 20; }\n";
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

        out << "const { climbYggdrasil } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 1101:
                out << "if (climbYggdrasil([1, 2, 3], 0) !== 3) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, -1, 3], 0) !== 2) allCorrect = false;\n";
                out << "if (climbYggdrasil([5], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 0) !== 7) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, -1, 3, -1], 0) !== 3) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const t = new Array(n).fill(1);\n";
                out << "    if (climbYggdrasil(t, 0) !== n) allCorrect = false;\n";
                out << "    for (let i = 0; i < n; i += 2) t[i] = -1;\n";
                out << "    let expected = 0;\n";
                out << "    for (let x of t) if (x !== -1) expected++;\n";
                out << "    if (climbYggdrasil(t, 0) !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 18; }\n";
                break;

            case 1102:
                out << "if (climbYggdrasil([1, 2, 3, 4, 5], 0) !== 3) allCorrect = false;\n";
                out << "if (climbYggdrasil([1], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, -1, 3, -1], 0) !== 3) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 0) !== 3) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const t = new Array(n).fill(1);\n";
                out << "    let depth = 0;\n";
                out << "    let tmp = n;\n";
                out << "    while (tmp > 0) { depth++; tmp = Math.floor(tmp / 2); }\n";
                out << "    if (climbYggdrasil(t, 0) !== depth) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 1103:
                out << "if (climbYggdrasil([1, 2, 3, 4, 5], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, -1, 3, -1], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, -1], 0) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const t = new Array(n).fill(1);\n";
                out << "    if (climbYggdrasil(t, 0) !== 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 17; }\n";
                break;

            case 1104:
                out << "if (climbYggdrasil([5, 4, 8, 11, -1, 13, 4, 7, 2], 22) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3], 4) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3], 5) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1], 1) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1], 2) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 7) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 9) !== 0) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const t = new Array(n).fill(1);\n";
                out << "    let depth = 0, tmp = n;\n";
                out << "    while (tmp > 0) { depth++; tmp = Math.floor(tmp / 2); }\n";
                out << "    if (climbYggdrasil(t, depth) !== 1) allCorrect = false;\n";
                out << "    if (climbYggdrasil(t, n * 2) !== 0) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 20; }\n";
                break;

            case 1105:
                out << "if (climbYggdrasil([1, 2, 3], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5], 0) !== 3) allCorrect = false;\n";
                out << "if (climbYggdrasil([5], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([10, 5, -1, 3, -1], 0) !== 15) allCorrect = false;\n";
                out << "if (climbYggdrasil([], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3, 4, 5, 6, 7], 0) !== 6) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const t = new Array(n).fill(1);\n";
                out << "    let nonLeaves = 0;\n";
                out << "    for (let i = 0; i < n; i++) {\n";
                out << "        const left = 2 * i + 1, right = 2 * i + 2;\n";
                out << "        if ((left < n && t[left] !== -1) || (right < n && t[right] !== -1)) nonLeaves++;\n";
                out << "    }\n";
                out << "    if (climbYggdrasil(t, 0) !== nonLeaves) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 17; }\n";
                break;

            case 1106:
                out << "if (climbYggdrasil([1, 2, 2, 3, 4, 4, 3], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 2, -1, 3, -1, 3], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([1], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 2], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 3], 0) !== 0) allCorrect = false;\n";
                out << "if (climbYggdrasil([], 0) !== 1) allCorrect = false;\n";
                out << "if (climbYggdrasil([1, 2, 2, 3, 4, 4, 3, 5, -1, -1, 6, 6, -1, -1, 5], 0) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = 1023;\n";
                out << "    const t = new Array(n).fill(0);\n";
                out << "    t[0] = 1;\n";
                out << "    for (let i = 0; i < n; i++) {\n";
                out << "        const left = 2 * i + 1, right = 2 * i + 2;\n";
                out << "        if (left < n) t[left] = i + 2;\n";
                out << "        if (right < n) t[right] = i + 2;\n";
                out << "    }\n";
                out << "    if (climbYggdrasil(t, 0) !== 1) allCorrect = false;\n";
                out << "    t[n - 1] = 999999;\n";
                out << "    if (climbYggdrasil(t, 0) !== 0) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 20; }\n";
                break;

            case 1107:
                out << "if (climbYggdrasil([4, 6, 2, 8, 0, 4, 7, 9], 0) !== 2) allCorrect = false;\n";
                out << "if (climbYggdrasil([8, 6, 2, 8, 0, 4, 7, 9], 0) !== 6) allCorrect = false;\n";
                out << "if (climbYggdrasil([4, 6, 2, 8, 0, 4, 7, 9], 7) !== 6) allCorrect = false;\n";
                out << "if (climbYggdrasil([9, 6, 2, 8, 0, 4, 7, 9], 7) !== 8) allCorrect = false;\n";
                out << "if (climbYggdrasil([5, 5], 5) !== 5) allCorrect = false;\n";
                out << "if (climbYggdrasil([2, 6, 2, 8, 0, 4, 7, 9], 0) !== 2) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = 1000;\n";
                out << "    const t = [500, 500, 250, 750, 125, 375, 625, 875];\n";
                out << "    if (climbYggdrasil(t, 125) !== 500) allCorrect = false;\n";
                out << "    t[0] = 500;\n";
                out << "    if (climbYggdrasil(t, 625) !== 500) allCorrect = false;\n";
                out << "    t[0] = 375;\n";
                out << "    if (climbYggdrasil(t, 125) !== 250) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 20; }\n";
                break;
        }

        out << "\nconst end = process.hrtime.bigint();\n";
        out << "const ms = Number((end - start) / 1000000n);\n";
        out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
        out << "process.exit(allCorrect ? 0 : 1);\n";

        out.close();
    }
}
