#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> clocktower::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1001, "The Smallest Gear", WORLD_NAME, WORLD_DESC,
        "Sum all elements of gears recursively. target is unused. Do not use a loop — solve with recursion only.",
        "Example: [1,2,3,4] -> 10\nExample: [5] -> 5\nExample: [] -> 0\nExample: [-1,1,-1,1] -> 0",
        "0 <= N <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int openClock(vector<int>& gears, int target)",
        "Base case: empty array returns 0. Recursive case: first element + recursive sum of rest. O(n) depth, O(n) time.", generateStub, generateRunner
    });

    waves.push_back({
        1002, "The Double Mechanism", WORLD_NAME, WORLD_DESC,
        "Return the target-th Fibonacci number. F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2). gears is unused. Two recursive calls whose results combine on return.",
        "Example: target=0 -> 0\nExample: target=1 -> 1\nExample: target=5 -> 5\nExample: target=10 -> 55",
        "0 <= target <= 40\ngears is unused.",
        40, 2000, 300,
        "int openClock(vector<int>& gears, int target)",
        "Base cases: F(0)=0, F(1)=1. Recursive case: F(n) = F(n-1) + F(n-2). Naive recursion is O(2^n) -- memoization or iterative approach makes it O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1003, "The Halving Spring", WORLD_NAME, WORLD_DESC,
        "Fast exponentiation: return gears[0] raised to the power target, using recursive halving. If target is even, compute half and square it. If odd, multiply by base and recurse on target-1.",
        "Example: gears=[2,...], target=10 -> 1024\nExample: gears=[3,...], target=0 -> 1\nExample: gears=[2,...], target=1 -> 2\nExample: gears=[5,...], target=3 -> 125",
        "1 <= gears[0] <= 10\n0 <= target <= 20\nResult fits in int.",
        20, 1000, 300,
        "int openClock(vector<int>& gears, int target)",
        "If exponent is 0, return 1. If even, compute half = power(base, exp/2) and return half*half. If odd, return base * power(base, exp-1). O(log n) depth.", generateStub, generateRunner
    });

    waves.push_back({
        1004, "The Nested Chamber", WORLD_NAME, WORLD_DESC,
        "Count the number of subsets of gears that sum to exactly target. Each element is either included or excluded — decisions flow down, count bubbles up.",
        "Example: [1,2,3], target=3 -> 2 ({1,2} and {3})\nExample: [1,1,1], target=2 -> 3\nExample: [5], target=5 -> 1\nExample: [1,2,3], target=7 -> 0",
        "0 <= N <= 20\n-100 <= gears[i] <= 100",
        20, 2000, 300,
        "int openClock(vector<int>& gears, int target)",
        "For each element, two choices: include it (subtract from target, recurse on rest) or exclude it (recurse on rest). Count paths that reach target=0 at base case. O(2^n).", generateStub, generateRunner
    });

    waves.push_back({
        1005, "The Preorder Clock", WORLD_NAME, WORLD_DESC,
        "gears encodes a binary tree in preorder. Format: [value, left_count, left_subtree..., right_subtree...]. left_count is how many elements belong to the left subtree. Return the sum of all node values. Trust the recursion to consume the right amount.",
        "Example: [5, 2, 3, 0, 7] -> 15 (root=5, left subtree has 2 elements: [3,0] meaning node 3 with 0-element left, then right=7)\nExample: [1, 0] -> 1 (leaf node)\nExample: [10, 4, 5, 2, 2, 0, 3, 0] -> 20 (10+5+2+3=20)",
        "1 <= nodes <= 1000\ngears encodes a valid tree.\ntarget is unused.",
        1000, 1000, 300,
        "int openClock(vector<int>& gears, int target)",
        "Parse node: read value, read left_count. Recursively parse left_count elements for left subtree, then parse remaining elements for right subtree. Return value + left_sum + right_sum. Trust the recursion to consume exactly the right number of elements.", generateStub, generateRunner
    });

    waves.push_back({
        1006, "The Bounds Within", WORLD_NAME, WORLD_DESC,
        "gears is a preorder traversal of a binary search tree. Return 1 if it represents a valid BST (every node's value is within the range inherited from its ancestors). Return 0 otherwise. Pass min/max bounds DOWN while the answer propagates UP.",
        "Example: [5, 3, 1, 4, 8, 7, 9] -> 1 (valid BST preorder)\nExample: [5, 3, 1, 6, 8, 7, 9] -> 0 (6 is in left subtree of 5 but 6>5)\nExample: [1] -> 1\nExample: [2, 1, 3] -> 1",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int openClock(vector<int>& gears, int target)",
        "Maintain min/max bounds as you recurse. First element is root. Find where right subtree starts (first element > root). Recursively validate left subtree with (min, root) bounds and right with (root, max) bounds. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a recursive approach",
        "Need self-similar subproblems with clear base cases and recursive transitions",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Recursive validation with inherited min/max bounds",
            "Sliding window over preorder values",
            "Heap of subtree candidates",
            "Union-Find over ancestor links"
        },
        0,
        "Need self-similar subproblems with clear base cases and recursive transitions",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given an array `gears`, recursively return the sum of all elements without using a loop.";
    waves[0].flavor_text = "Inside the Clocktower, an apprentice horologist listens to each brass tooth as it turns. The whole machine's weight is only the sum of smaller pieces, and she can understand it only by descending one gear at a time.";
    waves[1].clear_prompt = "Given integer `target`, return the `target`th Fibonacci number.";
    waves[1].flavor_text = "Some mechanisms in the tower are built from repetition and memory rather than metal. The apprentice traces the old doubling-and-return rhythm through the gears until the requested term finally appears.";
    waves[2].clear_prompt = "Given base `gears[0]` and exponent `target`, return `base^target` using recursive halving.";
    waves[2].flavor_text = "The tower's spring could be wound by brute force, but the apprentice knows a better rhythm: split the effort, square the result, and let repeated halving do the hard work.";
    waves[3].clear_prompt = "Given an array `gears` and integer `target`, return how many subsets of the array sum exactly to `target`.";
    waves[3].flavor_text = "Every gear can either join the mechanism or stay at rest. The apprentice explores both futures at each choice point, counting the branches that land on the exact total the tower demands.";
    waves[4].clear_prompt = "Given a preorder tree encoding in `gears` as described by the wave, return the sum of all node values.";
    waves[4].flavor_text = "The clock's blueprint is folded into a strange preorder chant. The apprentice must trust recursive descent to consume each subtree cleanly, or the whole machine's structure will dissolve into nonsense.";
    waves[5].clear_prompt = "Given a preorder traversal of a BST in `gears`, return `1` if it is valid and `0` otherwise.";
    waves[5].flavor_text = "The oldest clock in the tower obeys laws of less-than and greater-than as strict as any faith. The apprentice tests whether the reported preorder could truly have come from such a machine without violating its inherited bounds.";
    return waves;
}

void clocktower::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <climits>\n";
        out << "using namespace std;\n\n";

        out << "int openClock(vector<int>& gears, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 1001:
                out << "    {\n";
                out << "        vector<int> g = {1, 2, 3, 4};\n";
                out << "        if (openClock(g, 0) != 10) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {5};\n";
                out << "        if (openClock(g, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {};\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {-1, 1, -1, 1};\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {100, 200, 300};\n";
                out << "        if (openClock(g, 0) != 600) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> g(n, 1);\n";
                out << "        if (openClock(g, 0) != n) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 14; }\n";
                break;

            case 1002:
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 5) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 10) != 55) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 2) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 7) != 13) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<int> g;\n";
                out << "        if (openClock(g, 30) != 832040) all_correct = false;\n";
                out << "        if (openClock(g, 40) != 102334155) all_correct = false;\n";
                out << "        ops = 40;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 1003:
                out << "    {\n";
                out << "        vector<int> g = {2};\n";
                out << "        if (openClock(g, 10) != 1024) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {3};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {2};\n";
                out << "        if (openClock(g, 1) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {5};\n";
                out << "        if (openClock(g, 3) != 125) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {7};\n";
                out << "        if (openClock(g, 4) != 2401) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1};\n";
                out << "        if (openClock(g, 20) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<int> g = {2};\n";
                out << "        if (openClock(g, 20) != 1048576) all_correct = false;\n";
                out << "        vector<int> g2 = {3};\n";
                out << "        if (openClock(g2, 13) != 1594323) all_correct = false;\n";
                out << "        ops = 20;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 1004:
                out << "    {\n";
                out << "        vector<int> g = {1, 2, 3};\n";
                out << "        if (openClock(g, 3) != 2) all_correct = false; // {1,2} and {3}\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1, 1, 1};\n";
                out << "        if (openClock(g, 2) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {5};\n";
                out << "        if (openClock(g, 5) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1, 2, 3};\n";
                out << "        if (openClock(g, 7) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false; // empty set sums to 0\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1, 2, 3, 4, 5};\n";
                out << "        if (openClock(g, 5) != 3) all_correct = false; // {5}, {2,3}, {1,4}\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1, 1, 1, 1, 1};\n";
                out << "        if (openClock(g, 3) != 10) all_correct = false; // C(5,3)\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<int> g = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20};\n";
                out << "        // Subsets summing to 10: many possibilities\n";
                out << "        int r = openClock(g, 10);\n";
                out << "        if (r < 1) all_correct = false; // sanity: at least {10} works\n";
                out << "        // Subsets summing to 210 (total of 1..20): only one (the full set)\n";
                out << "        if (openClock(g, 210) != 1) all_correct = false;\n";
                out << "        ops = 20;\n";
                out << "    } else { ops = 20; }\n";
                break;

            case 1005:
                out << "    {\n";
                out << "        // Tree: root=5, left subtree has 2 elements [3, 0], right=7\n";
                out << "        // Node 3 has left_count=0 (no left child), right is empty too (consumed)\n";
                out << "        // Actually let me define format clearly:\n";
                out << "        // [value, left_count, ...left_elements..., ...right_elements...]\n";
                out << "        // Leaf: [value, 0] (0 elements in left subtree, right is rest)\n";
                out << "        // But right subtree size is implicit (rest of current allocation)\n";
                out << "        // Simple tree: just root=5, no children -> [5, 0]\n";
                out << "        vector<int> g = {5, 0};\n";
                out << "        if (openClock(g, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // root=5, left has 2 elements: [3, 0], right has 2 elements: [7, 0]\n";
                out << "        vector<int> g = {5, 2, 3, 0, 7, 0};\n";
                out << "        if (openClock(g, 0) != 15) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // root=1, left has 4 elements: [2, 2, 4, 0, 5, 0], right has 2: [3, 0]\n";
                out << "        // Left subtree: root=2, left has 2 elements [4,0], right [5,0]\n";
                out << "        vector<int> g = {1, 4, 2, 2, 4, 0, 5, 0, 3, 0};\n";
                out << "        if (openClock(g, 0) != 15) all_correct = false; // 1+2+4+5+3\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Linear tree: 1 -> 2 -> 3 (all right children)\n";
                out << "        vector<int> g = {1, 0, 2, 0, 3, 0};\n";
                out << "        if (openClock(g, 0) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        // Build a balanced-ish tree of ~500 nodes\n";
                out << "        // Simple: chain of right children, each node = [i, 0, ...]\n";
                out << "        int n = 500;\n";
                out << "        vector<int> g;\n";
                out << "        for (int i = 1; i <= n; i++) { g.push_back(i); g.push_back(0); }\n";
                out << "        int expected = n * (n + 1) / 2;\n";
                out << "        if (openClock(g, 0) != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 20; }\n";
                break;

            case 1006:
                out << "    {\n";
                out << "        vector<int> g = {5, 3, 1, 4, 8, 7, 9};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false; // valid BST\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {5, 3, 1, 6, 8, 7, 9};\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false; // 6 in left subtree of 5\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {1};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {2, 1, 3};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {2, 3, 1};\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false; // 3 in left of 2\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {10, 5, 3, 7, 15, 12, 20};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> g = {10, 5, 3, 7, 15, 12, 11};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false; // 11 is valid right of 12? No: 11 < 12 so it's left of 12, and 11 > 10 so valid\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Strictly increasing = valid (right-skewed BST)\n";
                out << "        vector<int> g = {1, 2, 3, 4, 5};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Strictly decreasing = valid (left-skewed BST)\n";
                out << "        vector<int> g = {5, 4, 3, 2, 1};\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        // Subtle invalid: [3, 1, 2, 5, 4, 0]\n";
                out << "        // 3 root, left subtree: 1,2 (valid), right subtree: 5,4,0\n";
                out << "        // In right: 5 root, left is 4 (valid >3, <5), right is 0 (invalid: 0 < 3)\n";
                out << "        vector<int> g = {3, 1, 2, 5, 4, 0};\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        // Build valid BST preorder: 1,2,3,...,n (right-skewed)\n";
                out << "        vector<int> g(n);\n";
                out << "        for (int i = 0; i < n; i++) g[i] = i + 1;\n";
                out << "        if (openClock(g, 0) != 1) all_correct = false;\n";
                out << "        // Break it: put 0 at the end (violates lower bound from ancestors)\n";
                out << "        g[n - 1] = 0;\n";
                out << "        if (openClock(g, 0) != 0) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 26; }\n";
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

        out << "const { openClock } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 1001:
                out << "if (openClock([1, 2, 3, 4], 0) !== 10) allCorrect = false;\n";
                out << "if (openClock([5], 0) !== 5) allCorrect = false;\n";
                out << "if (openClock([], 0) !== 0) allCorrect = false;\n";
                out << "if (openClock([-1, 1, -1, 1], 0) !== 0) allCorrect = false;\n";
                out << "if (openClock([100, 200, 300], 0) !== 600) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const g = new Array(n).fill(1);\n";
                out << "    if (openClock(g, 0) !== n) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 14; }\n";
                break;

            case 1002:
                out << "if (openClock([], 0) !== 0) allCorrect = false;\n";
                out << "if (openClock([], 1) !== 1) allCorrect = false;\n";
                out << "if (openClock([], 5) !== 5) allCorrect = false;\n";
                out << "if (openClock([], 10) !== 55) allCorrect = false;\n";
                out << "if (openClock([], 2) !== 1) allCorrect = false;\n";
                out << "if (openClock([], 7) !== 13) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    if (openClock([], 30) !== 832040) allCorrect = false;\n";
                out << "    if (openClock([], 40) !== 102334155) allCorrect = false;\n";
                out << "    ops = 40;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 1003:
                out << "if (openClock([2], 10) !== 1024) allCorrect = false;\n";
                out << "if (openClock([3], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([2], 1) !== 2) allCorrect = false;\n";
                out << "if (openClock([5], 3) !== 125) allCorrect = false;\n";
                out << "if (openClock([7], 4) !== 2401) allCorrect = false;\n";
                out << "if (openClock([1], 20) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    if (openClock([2], 20) !== 1048576) allCorrect = false;\n";
                out << "    if (openClock([3], 13) !== 1594323) allCorrect = false;\n";
                out << "    ops = 20;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 1004:
                out << "if (openClock([1, 2, 3], 3) !== 2) allCorrect = false;\n";
                out << "if (openClock([1, 1, 1], 2) !== 3) allCorrect = false;\n";
                out << "if (openClock([5], 5) !== 1) allCorrect = false;\n";
                out << "if (openClock([1, 2, 3], 7) !== 0) allCorrect = false;\n";
                out << "if (openClock([], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([1, 2, 3, 4, 5], 5) !== 3) allCorrect = false;\n";
                out << "if (openClock([1, 1, 1, 1, 1], 3) !== 10) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const g = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];\n";
                out << "    const r = openClock(g, 10);\n";
                out << "    if (r < 1) allCorrect = false;\n";
                out << "    if (openClock(g, 210) !== 1) allCorrect = false;\n";
                out << "    ops = 20;\n";
                out << "} else { ops = 20; }\n";
                break;

            case 1005:
                out << "if (openClock([5, 0], 0) !== 5) allCorrect = false;\n";
                out << "if (openClock([5, 2, 3, 0, 7, 0], 0) !== 15) allCorrect = false;\n";
                out << "if (openClock([1, 4, 2, 2, 4, 0, 5, 0, 3, 0], 0) !== 15) allCorrect = false;\n";
                out << "if (openClock([1, 0, 2, 0, 3, 0], 0) !== 6) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = 500;\n";
                out << "    const g = [];\n";
                out << "    for (let i = 1; i <= n; i++) { g.push(i); g.push(0); }\n";
                out << "    const expected = n * (n + 1) / 2;\n";
                out << "    if (openClock(g, 0) !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 20; }\n";
                break;

            case 1006:
                out << "if (openClock([5, 3, 1, 4, 8, 7, 9], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([5, 3, 1, 6, 8, 7, 9], 0) !== 0) allCorrect = false;\n";
                out << "if (openClock([1], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([2, 1, 3], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([2, 3, 1], 0) !== 0) allCorrect = false;\n";
                out << "if (openClock([10, 5, 3, 7, 15, 12, 20], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([10, 5, 3, 7, 15, 12, 11], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([1, 2, 3, 4, 5], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([5, 4, 3, 2, 1], 0) !== 1) allCorrect = false;\n";
                out << "if (openClock([3, 1, 2, 5, 4, 0], 0) !== 0) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const g = [];\n";
                out << "    for (let i = 0; i < n; i++) g.push(i + 1);\n";
                out << "    if (openClock(g, 0) !== 1) allCorrect = false;\n";
                out << "    g[n - 1] = 0;\n";
                out << "    if (openClock(g, 0) !== 0) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 26; }\n";
                break;
        }

        out << "\nconst end = process.hrtime.bigint();\n";
        out << "const ms = Number((end - start) / 1000000n);\n";
        out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
        out << "process.exit(allCorrect ? 0 : 1);\n";

        out.close();
    }
}
