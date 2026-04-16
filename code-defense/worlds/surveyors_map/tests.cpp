#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> surveyors_map::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2101, "Count Territories", WORLD_NAME, WORLD_DESC,
        "Count the number of connected components. target = number of island nodes (0-indexed). connections = list of [a, b] edges connecting islands. Use Union-Find to group connected nodes, then count distinct groups.",
        "Example: [[0,1],[1,2]], target=4 -> 2 (nodes 0-1-2 connected, node 3 alone)\nExample: [[0,1],[2,3]], target=4 -> 2\nExample: [], target=3 -> 3\nExample: [[0,1],[0,2],[1,2]], target=3 -> 1",
        "0 <= target <= 100,000\n0 <= connections[i][j] < target",
        100000, 1000, 300,
        "int surveyLand(vector<vector<int>>& connections, int target)",
        "Union-Find with path compression and union by rank. Process each edge as a union. Final answer = target - number of successful unions. O(n * alpha(n)) which is nearly O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2102, "Are They Connected?", WORLD_NAME, WORLD_DESC,
        "connections[0] = [a, b] is a query pair. connections[1..] are the edges. Return 1 if nodes a and b are in the same connected component, 0 otherwise. target = number of nodes.",
        "Example: [[0,2],[0,1],[1,2]], target=3 -> 1 (query 0-2, edges 0-1 and 1-2 connect them)\nExample: [[0,2],[0,1]], target=3 -> 0 (query 0-2, only edge 0-1, node 2 unreachable)\nExample: [[0,0]], target=1 -> 1 (query node 0 to itself)",
        "1 <= target <= 100,000\nconnections has at least 1 element (the query)",
        100000, 1000, 300,
        "int surveyLand(vector<vector<int>>& connections, int target)",
        "Build Union-Find from edges (skip first edge which is the query). Find representatives of the two query nodes. Same representative = connected. O(n * alpha(n)).", generateStub, generateRunner
    });

    waves.push_back({
        2103, "Redundant Bridge", WORLD_NAME, WORLD_DESC,
        "Find the last edge in connections that, when added, creates a cycle (the redundant edge). Return its 0-based index. There is guaranteed to be exactly one redundant edge. target = number of nodes.",
        "Example: [[0,1],[1,2],[2,0]], target=3 -> 2 (edge [2,0] closes the cycle)\nExample: [[0,1],[0,2],[2,1]], target=3 -> 2\nExample: [[0,1],[1,2],[0,2],[2,3]], target=4 -> 2",
        "target >= 1\nExactly one cycle exists.\nconnections forms a tree plus one extra edge.",
        100000, 1000, 300,
        "int surveyLand(vector<vector<int>>& connections, int target)",
        "Process edges in order with Union-Find. The first edge where both endpoints are already in the same set is redundant (creates a cycle). Return that edge's index. O(n * alpha(n)).", generateStub, generateRunner
    });

    waves.push_back({
        2104, "Earliest Connection", WORLD_NAME, WORLD_DESC,
        "Connections arrive in order. Return the 0-based index of the edge at which all target nodes first become fully connected. Return -1 if they never all connect.",
        "Example: [[0,1],[1,2]], target=3 -> 1 (after edge at index 1, all 3 nodes connected)\nExample: [[0,1],[2,3],[0,2]], target=4 -> 2\nExample: [[0,1]], target=3 -> -1 (node 2 never connected)",
        "1 <= target <= 100,000\n0 <= connections[i][j] < target",
        1000, 1000, 300,
        "int surveyLand(vector<vector<int>>& connections, int target)",
        "Process edges with Union-Find, tracking component count. Start with target components. Each successful union decreases count by 1. When count reaches 1, return current index. O(n * alpha(n)).", generateStub, generateRunner
    });

    waves.push_back({
        2105, "The Great Merge (boss)", WORLD_NAME, WORLD_DESC,
        "connections[i] = [a, b] means account a and account b belong to the same person. target = total number of accounts. Return the number of distinct people (connected components). Validates full union-find with path compression and union by rank.",
        "Example: [[0,1],[1,2],[3,4]], target=6 -> 3 (groups: {0,1,2}, {3,4}, {5})\nExample: [[0,1],[1,2],[2,3],[3,4]], target=5 -> 1\nExample: [], target=5 -> 5",
        "0 <= target <= 500,000\n0 <= connections[i][j] < target",
        500000, 2000, 300,
        "int surveyLand(vector<vector<int>>& connections, int target)",
        "Standard Union-Find with path compression and union by rank. Process all edges. Count remaining distinct root representatives. O(n * alpha(n)).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a union-find approach",
        "Need to maintain dynamic connectivity under repeated union operations",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Union-Find with path compression and union by rank",
            "Breadth-first traversal from every node",
            "Heap of lightest connections",
            "Sliding window over edge arrivals"
        },
        0,
        "Need to maintain dynamic connectivity under repeated union operations",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given `connections` as edge pairs and `target` as the number of nodes, return the number of connected components.";
    waves[0].flavor_text = "Across the frontier ledger, the chief surveyor marks which lands are already tied together by rope bridges and which still stand isolated in the mist.";
    waves[1].clear_prompt = "Given `connections` where the first pair is a query and the remaining pairs are edges, return `1` if the queried nodes are connected and `0` otherwise.";
    waves[1].flavor_text = "The surveyor is handed a map and one urgent question: do these two points belong to the same hidden territory, or does broken ground still lie between them?";
    waves[2].clear_prompt = "Given `connections` as edge pairs, return the index of the edge that first creates a cycle, as specified by the wave.";
    waves[2].flavor_text = "A redundant bridge has been laid across the map, and now the routes fold back on themselves. The surveyor wants the exact moment the frontier ceased to be a tree and became a loop.";
    waves[3].clear_prompt = "Given `connections` arriving in order and `target` nodes, return the first index at which all nodes become fully connected, or `-1` if they never do.";
    waves[3].flavor_text = "Each new edge is another promise that the frontier might finally join into one whole country. The surveyor waits for the exact instant when the last isolation disappears.";
    waves[4].clear_prompt = "Given `connections` and total account count `target`, return the number of distinct connected groups using Union-Find.";
    waves[4].flavor_text = "At the end of the survey, names, routes, and ownership claims must collapse into their true underlying groups. The surveyor compresses the frontier's many aliases into the smallest honest count of people.";
    return waves;
}

void surveyors_map::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int surveyLand(vector<vector<int>>& connections, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 2101:
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2}};\n";
            out << "        if (surveyLand(c, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{2,3}};\n";
            out << "        if (surveyLand(c, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {};\n";
            out << "        if (surveyLand(c, 3) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{0,2},{1,2}};\n";
            out << "        if (surveyLand(c, 3) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1}};\n";
            out << "        if (surveyLand(c, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> c;\n";
            out << "        for (int i = 0; i < n - 1; i++) c.push_back({i, i + 1});\n";
            out << "        if (surveyLand(c, n) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2102:
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,2},{0,1},{1,2}};\n";
            out << "        if (surveyLand(c, 3) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,2},{0,1}};\n";
            out << "        if (surveyLand(c, 3) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,0}};\n";
            out << "        if (surveyLand(c, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{1,3},{0,1},{1,2},{2,3}};\n";
            out << "        if (surveyLand(c, 4) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,4},{0,1},{1,2},{3,4}};\n";
            out << "        if (surveyLand(c, 5) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> c;\n";
            out << "        c.push_back({0, n - 1}); // query: are 0 and n-1 connected?\n";
            out << "        for (int i = 0; i < n - 1; i++) c.push_back({i, i + 1});\n";
            out << "        if (surveyLand(c, n) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 2103:
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2},{2,0}};\n";
            out << "        if (surveyLand(c, 3) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{0,2},{2,1}};\n";
            out << "        if (surveyLand(c, 3) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2},{0,2},{2,3}};\n";
            out << "        if (surveyLand(c, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2},{2,3},{3,0}};\n";
            out << "        if (surveyLand(c, 4) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{0,1}};\n";
            out << "        if (surveyLand(c, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> c;\n";
            out << "        for (int i = 0; i < n - 1; i++) c.push_back({i, i + 1});\n";
            out << "        c.push_back({0, n - 1}); // redundant edge at the end\n";
            out << "        if (surveyLand(c, n) != n - 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2104:
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2}};\n";
            out << "        if (surveyLand(c, 3) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{2,3},{0,2}};\n";
            out << "        if (surveyLand(c, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1}};\n";
            out << "        if (surveyLand(c, 3) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{0,2},{0,3},{0,4}};\n";
            out << "        if (surveyLand(c, 5) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1}};\n";
            out << "        if (surveyLand(c, 2) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> c;\n";
            out << "        for (int i = 0; i < n - 1; i++) c.push_back({i, i + 1});\n";
            out << "        if (surveyLand(c, n) != n - 2) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 2105:
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2},{3,4}};\n";
            out << "        if (surveyLand(c, 6) != 3) all_correct = false; // {0,1,2}, {3,4}, {5}\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{1,2},{2,3},{3,4}};\n";
            out << "        if (surveyLand(c, 5) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {};\n";
            out << "        if (surveyLand(c, 5) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{2,3},{4,5},{0,2},{4,0}};\n";
            out << "        if (surveyLand(c, 6) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> c = {{0,1},{0,1},{0,1}};\n";
            out << "        if (surveyLand(c, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> c;\n";
            out << "        for (int i = 0; i < n - 1; i++) c.push_back({i, i + 1});\n";
            out << "        // Add redundant edges to stress path compression\n";
            out << "        for (int i = 0; i < n / 2; i++) c.push_back({i, n - 1 - i});\n";
            out << "        if (surveyLand(c, n) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
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

    out << "const { surveyLand } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 2101:
            out << "{\n";
            out << "    let c = [[0,1],[1,2]];\n";
            out << "    if (surveyLand(c, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[2,3]];\n";
            out << "    if (surveyLand(c, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [];\n";
            out << "    if (surveyLand(c, 3) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[0,2],[1,2]];\n";
            out << "    if (surveyLand(c, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1]];\n";
            out << "    if (surveyLand(c, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [];\n";
            out << "    for (let i = 0; i < n - 1; i++) c.push([i, i + 1]);\n";
            out << "    if (surveyLand(c, n) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 2102:
            out << "{\n";
            out << "    let c = [[0,2],[0,1],[1,2]];\n";
            out << "    if (surveyLand(c, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,2],[0,1]];\n";
            out << "    if (surveyLand(c, 3) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,0]];\n";
            out << "    if (surveyLand(c, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[1,3],[0,1],[1,2],[2,3]];\n";
            out << "    if (surveyLand(c, 4) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,4],[0,1],[1,2],[3,4]];\n";
            out << "    if (surveyLand(c, 5) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [[0, n - 1]];\n";
            out << "    for (let i = 0; i < n - 1; i++) c.push([i, i + 1]);\n";
            out << "    if (surveyLand(c, n) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 2103:
            out << "{\n";
            out << "    let c = [[0,1],[1,2],[2,0]];\n";
            out << "    if (surveyLand(c, 3) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[0,2],[2,1]];\n";
            out << "    if (surveyLand(c, 3) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[1,2],[0,2],[2,3]];\n";
            out << "    if (surveyLand(c, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[1,2],[2,3],[3,0]];\n";
            out << "    if (surveyLand(c, 4) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[0,1]];\n";
            out << "    if (surveyLand(c, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [];\n";
            out << "    for (let i = 0; i < n - 1; i++) c.push([i, i + 1]);\n";
            out << "    c.push([0, n - 1]);\n";
            out << "    if (surveyLand(c, n) !== n - 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 2104:
            out << "{\n";
            out << "    let c = [[0,1],[1,2]];\n";
            out << "    if (surveyLand(c, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[2,3],[0,2]];\n";
            out << "    if (surveyLand(c, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1]];\n";
            out << "    if (surveyLand(c, 3) !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[0,2],[0,3],[0,4]];\n";
            out << "    if (surveyLand(c, 5) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1]];\n";
            out << "    if (surveyLand(c, 2) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [];\n";
            out << "    for (let i = 0; i < n - 1; i++) c.push([i, i + 1]);\n";
            out << "    if (surveyLand(c, n) !== n - 2) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 2105:
            out << "{\n";
            out << "    let c = [[0,1],[1,2],[3,4]];\n";
            out << "    if (surveyLand(c, 6) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[1,2],[2,3],[3,4]];\n";
            out << "    if (surveyLand(c, 5) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [];\n";
            out << "    if (surveyLand(c, 5) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[2,3],[4,5],[0,2],[4,0]];\n";
            out << "    if (surveyLand(c, 6) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [[0,1],[0,1],[0,1]];\n";
            out << "    if (surveyLand(c, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [];\n";
            out << "    for (let i = 0; i < n - 1; i++) c.push([i, i + 1]);\n";
            out << "    for (let i = 0; i < Math.floor(n / 2); i++) c.push([i, n - 1 - i]);\n";
            out << "    if (surveyLand(c, n) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
