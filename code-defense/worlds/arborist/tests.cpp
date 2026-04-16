#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> arborist::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2301, "Root to Leaf Max", WORLD_NAME, WORLD_DESC,
        "Return the maximum root-to-leaf path sum. branches is a level-order binary tree array (children of i at 2i+1, 2i+2; -1 = null). target is unused.",
        "Example: [1,2,3] -> 4 (1+3)\nExample: [5,4,8,11,-1,13,4,7,2,-1,-1,-1,-1,-1,1] -> 27 (5->4->11->7)\nExample: [-10,9,20,-1,-1,15,7] -> 25 (-10->20->15)",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int pruneTree(vector<int>& branches, int target)",
        "DFS returning max path sum from current node to any leaf. At each node: value + max(left_max, right_max). For null nodes return 0. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2302, "Rob the Branches", WORLD_NAME, WORLD_DESC,
        "House robber on a tree. You cannot rob a node and its direct parent/children. Return the maximum sum you can rob. branches is a level-order binary tree array (-1 = null). target is unused.",
        "Example: [3,2,3,-1,3,-1,1] -> 7 (3+3+1)\nExample: [3,4,5,1,3,-1,1] -> 9 (4+5)\nExample: [1] -> 1",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int pruneTree(vector<int>& branches, int target)",
        "Tree DP with two states per node: robbed and not-robbed. rob(node) = value + not_rob(left) + not_rob(right). not_rob(node) = max(rob(left), not_rob(left)) + max(rob(right), not_rob(right)). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2303, "The Diameter", WORLD_NAME, WORLD_DESC,
        "Return the diameter of the tree — the longest path between any two nodes measured in edges. branches is a level-order binary tree array (-1 = null). target is unused.",
        "Example: [1,2,3,4,5] -> 3\nExample: [1,2,3,4,5,6,7] -> 4 (leaf to leaf through root)\nExample: [1] -> 0\nExample: [1,2,-1,3,-1] -> 2",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int pruneTree(vector<int>& branches, int target)",
        "DFS returning depth. At each node, diameter through this node = left_depth + right_depth. Track global maximum. Return max(left_depth, right_depth) + 1 up. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2304, "The Golden Branch", WORLD_NAME, WORLD_DESC,
        "Boss wave. Maximum path sum where the path can start and end at any node (not just root-to-leaf). This is the classic binary tree maximum path sum. branches is a level-order binary tree array (-1 = null). target is unused.",
        "Example: [1,2,3] -> 6 (2+1+3)\nExample: [-10,9,20,-1,-1,15,7] -> 42 (15+20+7)\nExample: [-3] -> -3\nExample: [2,-1] -> 2",
        "1 <= N <= 10,000\nValues can be negative.\ntarget is unused.",
        10000, 1000, 300,
        "int pruneTree(vector<int>& branches, int target)",
        "DFS returning max single-path sum from node downward. At each node, consider: left path, right path, or both through this node. Update global max with left+node+right. Return node + max(left, right, 0) upward. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a tree-recursive DP approach",
        "Need to combine results from child subtrees into a parent decision",
        "O(h) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Tree DFS with a global best plus downward path return values",
            "Breadth-first traversal by levels",
            "Binary search on the answer",
            "Union-Find over parent-child edges"
        },
        0,
        "Need to combine results from child subtrees into a parent decision",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(h) space")
    );
    waves[0].clear_prompt = "Given a binary tree encoded as a level-order array with `-1` as null, return the maximum root-to-leaf path sum.";
    waves[0].flavor_text = "The orchard's keeper climbs the oldest tree at dawn, palm on bark, listening for the richest flow of sap from root to leaf. One path through the branches carries the greatest yield, and that is the one he must mark for harvest.";
    waves[1].clear_prompt = "Given a binary tree encoded as a level-order array with `-1` as null, return the maximum value you can collect if no chosen node is directly adjacent to another chosen node.";
    waves[1].flavor_text = "Warding bells have been tied through the orchard to stop thieves. The keeper can strip fruit from a branch or from its children, but never both in the same chain, and he wants the most value the tree will allow without ringing the alarm.";
    waves[2].clear_prompt = "Given a binary tree encoded as a level-order array with `-1` as null, return the diameter of the tree measured in edges.";
    waves[2].flavor_text = "Storm damage has torn the canopy into long, dangerous arcs of living wood. The keeper wants the greatest span a climber could still cross through the tree without ever leaving bark for empty air.";
    waves[3].clear_prompt = "Given a binary tree encoded as a level-order array with `-1` as null, return the maximum path sum where the path may start and end at any nodes.";
    waves[3].flavor_text = "At the heart of the orchard, the tree's strength runs in strange directions, not only from trunk to leaf but branch to branch like old memory. The keeper seeks the single richest path anywhere in the canopy, no matter where it starts or ends.";
    return waves;
}

void arborist::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int pruneTree(vector<int>& branches, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 2301:
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3};\n";
            out << "        if (pruneTree(b, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {5, 4, 8, 11, -1, 13, 4, 7, 2, -1, -1, -1, -1, -1, 1};\n";
            out << "        if (pruneTree(b, 0) != 27) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {-10, 9, 20, -1, -1, 15, 7};\n";
            out << "        if (pruneTree(b, 0) != 25) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {10};\n";
            out << "        if (pruneTree(b, 0) != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, -1};\n";
            out << "        if (pruneTree(b, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> b(n, 1);\n";
            out << "        int depth = 0;\n";
            out << "        int tmp = n;\n";
            out << "        while (tmp > 1) { tmp /= 2; depth++; }\n";
            out << "        if (pruneTree(b, 0) != depth + 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2302:
            out << "    {\n";
            out << "        vector<int> b = {3, 2, 3, -1, 3, -1, 1};\n";
            out << "        if (pruneTree(b, 0) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {3, 4, 5, 1, 3, -1, 1};\n";
            out << "        if (pruneTree(b, 0) != 9) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1};\n";
            out << "        if (pruneTree(b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {4, 1, 1};\n";
            out << "        if (pruneTree(b, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 5, 5};\n";
            out << "        if (pruneTree(b, 0) != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> b(n, 1);\n";
            out << "        int result = pruneTree(b, 0);\n";
            out << "        if (result <= 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2303:
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3, 4, 5};\n";
            out << "        if (pruneTree(b, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3, 4, 5, 6, 7};\n";
            out << "        if (pruneTree(b, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1};\n";
            out << "        if (pruneTree(b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, -1, 3, -1};\n";
            out << "        if (pruneTree(b, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, -1};\n";
            out << "        if (pruneTree(b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> b(n, 1);\n";
            out << "        int depth = 0;\n";
            out << "        int tmp = n;\n";
            out << "        while (tmp > 1) { tmp /= 2; depth++; }\n";
            out << "        if (pruneTree(b, 0) != 2 * depth) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 2304:
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3};\n";
            out << "        if (pruneTree(b, 0) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {-10, 9, 20, -1, -1, 15, 7};\n";
            out << "        if (pruneTree(b, 0) != 42) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {-3};\n";
            out << "        if (pruneTree(b, 0) != -3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {2, -1};\n";
            out << "        if (pruneTree(b, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {5, 4, 8, -1, -1, 3, 2};\n";
            out << "        if (pruneTree(b, 0) != 20) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> b(n);\n";
            out << "        for (int i = 0; i < n; i++) b[i] = (i % 2 == 0) ? 10 : -1;\n";
            out << "        int result = pruneTree(b, 0);\n";
            out << "        if (result <= 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
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

    out << "const { pruneTree } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 2301:
            out << "{\n";
            out << "    let b = [1, 2, 3];\n";
            out << "    if (pruneTree(b, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [5, 4, 8, 11, -1, 13, 4, 7, 2, -1, -1, -1, -1, -1, 1];\n";
            out << "    if (pruneTree(b, 0) !== 27) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [-10, 9, 20, -1, -1, 15, 7];\n";
            out << "    if (pruneTree(b, 0) !== 25) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [10];\n";
            out << "    if (pruneTree(b, 0) !== 10) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, -1];\n";
            out << "    if (pruneTree(b, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let b = new Array(n).fill(1);\n";
            out << "    let depth = 0;\n";
            out << "    let tmp = n;\n";
            out << "    while (tmp > 1) { tmp = Math.floor(tmp / 2); depth++; }\n";
            out << "    if (pruneTree(b, 0) !== depth + 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 2302:
            out << "{\n";
            out << "    let b = [3, 2, 3, -1, 3, -1, 1];\n";
            out << "    if (pruneTree(b, 0) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [3, 4, 5, 1, 3, -1, 1];\n";
            out << "    if (pruneTree(b, 0) !== 9) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1];\n";
            out << "    if (pruneTree(b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [4, 1, 1];\n";
            out << "    if (pruneTree(b, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 5, 5];\n";
            out << "    if (pruneTree(b, 0) !== 10) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let b = new Array(n).fill(1);\n";
            out << "    let result = pruneTree(b, 0);\n";
            out << "    if (result <= 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 2303:
            out << "{\n";
            out << "    let b = [1, 2, 3, 4, 5];\n";
            out << "    if (pruneTree(b, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, 3, 4, 5, 6, 7];\n";
            out << "    if (pruneTree(b, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1];\n";
            out << "    if (pruneTree(b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, -1, 3, -1];\n";
            out << "    if (pruneTree(b, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, -1];\n";
            out << "    if (pruneTree(b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let b = new Array(n).fill(1);\n";
            out << "    let depth = 0;\n";
            out << "    let tmp = n;\n";
            out << "    while (tmp > 1) { tmp = Math.floor(tmp / 2); depth++; }\n";
            out << "    if (pruneTree(b, 0) !== 2 * depth) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 2304:
            out << "{\n";
            out << "    let b = [1, 2, 3];\n";
            out << "    if (pruneTree(b, 0) !== 6) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [-10, 9, 20, -1, -1, 15, 7];\n";
            out << "    if (pruneTree(b, 0) !== 42) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [-3];\n";
            out << "    if (pruneTree(b, 0) !== -3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [2, -1];\n";
            out << "    if (pruneTree(b, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [5, 4, 8, -1, -1, 3, 2];\n";
            out << "    if (pruneTree(b, 0) !== 20) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let b = [];\n";
            out << "    for (let i = 0; i < n; i++) b.push((i % 2 === 0) ? 10 : -1);\n";
            out << "    let result = pruneTree(b, 0);\n";
            out << "    if (result <= 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
