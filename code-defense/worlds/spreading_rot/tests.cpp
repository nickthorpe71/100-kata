#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> spreading_rot::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        601, "First Tendril", WORLD_NAME, WORLD_DESC,
        "Process the grid row by row, left to right (FIFO order). Return the sum of all non-zero cells visited in that order, but stop as soon as you hit a cell with value equal to target. If target is never hit, return the total sum.",
        "Example: [[1,2],[3,4]], target=3 -> 6 (visit 1,2,3 then stop: 1+2+3)\nExample: [[1,2],[3,4]], target=99 -> 10 (visit all)\nExample: [[0,0],[0,5]], target=5 -> 5",
        "1 <= rows, cols <= 500\n0 <= grid[i][j] <= 1000",
        500, 1000, 300,
        "int spreadRot(vector<vector<int>>& grid, int target)",
        "Iterate the grid row by row, left to right, accumulating the sum. Stop as soon as the target value is hit. Basic FIFO traversal order. O(rows*cols).", generateStub, generateRunner
    });

    waves.push_back({
        602, "Spread the Spores", WORLD_NAME, WORLD_DESC,
        "Grid: 2 = rotten, 1 = fresh, 0 = empty. Each minute, rotten cells infect adjacent fresh cells (up/down/left/right). Return the number of minutes until no fresh cells remain. Return -1 if impossible.",
        "Example: [[2,1,1],[1,1,0],[0,1,1]] -> 4\nExample: [[2,1,1],[0,1,1],[1,0,1]] -> -1\nExample: [[0,2]] -> 0\ntarget is unused.",
        "1 <= rows, cols <= 500",
        500, 2000, 300,
        "int spreadRot(vector<vector<int>>& grid, int target)",
        "Multi-source BFS. Enqueue all initially rotten cells. Process layer by layer, counting minutes. If fresh cells remain after BFS completes, return -1. O(rows*cols).", generateStub, generateRunner
    });

    waves.push_back({
        603, "Consume Layer by Layer", WORLD_NAME, WORLD_DESC,
        "Grid represents a binary tree in level-order (row 0 = root, -1 = null). Return the sum of values at level target (0-indexed). Process level by level using a queue.",
        "Example: [[3],[9,20],[-1,-1,15,7]], target=2 -> 22 (level 2: 15+7)\nExample: [[1]], target=0 -> 1\nExample: [[1],[2,3],[4,5,6,7]], target=1 -> 5 (2+3)",
        "1 <= nodes <= 100,000\n0 <= target < tree depth",
        100000, 1000, 300,
        "int spreadRot(vector<vector<int>>& grid, int target)",
        "BFS level-order traversal. Process each level as a batch -- dequeue all nodes at the current level, sum their values, and enqueue their children. Return the sum at the target level. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        604, "Shortest Path", WORLD_NAME, WORLD_DESC,
        "Grid: 0 = passable, 1 = wall. Find shortest path from top-left (0,0) to bottom-right (rows-1, cols-1). Move up/down/left/right. Return the path length (number of cells visited including start and end). Return -1 if no path exists.",
        "Example: [[0,0,0],[1,1,0],[0,0,0]] -> 5\nExample: [[0,1],[1,0]] -> -1\nExample: [[0]] -> 1\ntarget is unused.",
        "1 <= rows, cols <= 500",
        500, 2000, 300,
        "int spreadRot(vector<vector<int>>& grid, int target)",
        "BFS from (0,0). Each cell explored at distance d discovers neighbors at d+1. The first time we reach the target cell gives the shortest path. O(rows*cols).", generateStub, generateRunner
    });

    waves.push_back({
        605, "The Awakening", WORLD_NAME, WORLD_DESC,
        "Walls and gates: 0 = gate, -1 = wall, 2147483647 (INT_MAX) = empty room. Fill each empty room with the distance to its nearest gate (BFS from all gates simultaneously). Return the sum of all final room distances. Walls and gates keep their values.",
        "Example: [[2147483647,-1,0,2147483647],[2147483647,2147483647,2147483647,-1],[2147483647,-1,2147483647,-1],[0,-1,2147483647,2147483647]] -> 1+3+1+2+1+2+3 should be... let me verify: standard walls-and-gates result.\ntarget is unused.",
        "1 <= rows, cols <= 500",
        500, 2000, 300,
        "int spreadRot(vector<vector<int>>& grid, int target)",
        "Multi-source BFS from all gates (value 0). Process outward layer by layer, setting each empty room's distance. Rooms closer to any gate get the minimum distance automatically. O(rows*cols).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a queue-based BFS approach",
        "Need FIFO frontier processing to expand level by level",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Queue-based breadth-first search",
            "Binary search on infection time",
            "Union-Find over reachable cells",
            "Heap of currently rotten cells"
        },
        0,
        "Need FIFO frontier processing to expand level by level",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given a graph or grid as defined by the wave, perform the first required breadth-first expansion and return the requested count or value.";
    waves[0].flavor_text = "Rot travels outward in rings, and the plague warden watches the first tendrils push through a ruined field. He tracks the corruption in the order it spreads, because distance matters more than fear.";
    waves[1].clear_prompt = "Given a grid with fresh and rotten cells as defined by the wave, return the time required for the rot to spread everywhere it can, or the wave-specific failure value if some cells are unreachable.";
    waves[1].flavor_text = "The plague warden stands on a stone post above a field of spoiled fruit and watches the sickness leap from cell to cell. Each minute births a new frontier, and the last clean pocket decides whether the harvest can still be saved.";
    waves[2].clear_prompt = "Given a tree, graph, or grid, return the level-order or layered traversal result required by the wave.";
    waves[2].flavor_text = "The corruption does not move all at once. It arrives in patient layers, and the plague warden counts each wave as it reaches the next ring of the world.";
    waves[3].clear_prompt = "Given an unweighted graph or grid, return the shortest path length from the source to the target, or the wave-specific failure value if unreachable.";
    waves[3].flavor_text = "Some paths through the ruin are faster than others, and the plague warden cannot afford to wander. He expands outward evenly, one frontier at a time, until the destination finally answers back.";
    waves[4].clear_prompt = "Given the boss wave's graph or grid, solve the full spread or shortest-path problem using breadth-first traversal and return the required final value.";
    waves[4].flavor_text = "By the time the awakening begins, the field is no longer a puzzle but a siege. The plague warden must map the exact order of expansion or be swallowed with the rest of the living things.";
    return waves;
}

void spreading_rot::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <vector>\n";
    out << "#include <string>\n";
    out << "#include <queue>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <climits>\n";
    out << "#include <algorithm>\n";
    out << "using namespace std;\n\n";

    out << "int spreadRot(vector<vector<int>>& grid, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 601:
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1, 2}, {3, 4}};\n";
            out << "        if (spreadRot(g, 3) != 6) all_correct = false; // 1+2+3, stop at 3\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1, 2}, {3, 4}};\n";
            out << "        if (spreadRot(g, 99) != 10) all_correct = false; // visit all\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0, 0}, {0, 5}};\n";
            out << "        if (spreadRot(g, 5) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{7}};\n";
            out << "        if (spreadRot(g, 7) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};\n";
            out << "        if (spreadRot(g, 5) != 15) all_correct = false; // 1+2+3+4+5\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> g(n, vector<int>(n, 1));\n";
            out << "        if (spreadRot(g, 99) != n * n) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 602:
            out << "    {\n";
            out << "        vector<vector<int>> g = {{2,1,1},{1,1,0},{0,1,1}};\n";
            out << "        if (spreadRot(g, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{2,1,1},{0,1,1},{1,0,1}};\n";
            out << "        if (spreadRot(g, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0, 2}};\n";
            out << "        if (spreadRot(g, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{2, 1, 1, 1, 1}};\n";
            out << "        if (spreadRot(g, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{2}};\n";
            out << "        if (spreadRot(g, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1}};\n";
            out << "        if (spreadRot(g, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> g(n, vector<int>(n, 1));\n";
            out << "        g[0][0] = 2;\n";
            out << "        if (spreadRot(g, 0) != 2 * (n - 1)) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 603:
            out << "    {\n";
            out << "        vector<vector<int>> g = {{3}, {9, 20}, {-1, -1, 15, 7}};\n";
            out << "        if (spreadRot(g, 2) != 22) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1}};\n";
            out << "        if (spreadRot(g, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1}, {2, 3}, {4, 5, 6, 7}};\n";
            out << "        if (spreadRot(g, 1) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1}, {2, 3}, {4, 5, 6, 7}};\n";
            out << "        if (spreadRot(g, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1}, {2, -1}, {3, -1, -1, -1}};\n";
            out << "        if (spreadRot(g, 1) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int levels = 15;\n";
            out << "        vector<vector<int>> g;\n";
            out << "        int val = 1;\n";
            out << "        for (int l = 0; l < levels; l++) {\n";
            out << "            int count = 1 << l;\n";
            out << "            vector<int> row;\n";
            out << "            for (int i = 0; i < count; i++) row.push_back(val++);\n";
            out << "            g.push_back(row);\n";
            out << "        }\n";
            out << "        if (spreadRot(g, 0) != 1) all_correct = false;\n";
            out << "        if (spreadRot(g, 1) != 5) all_correct = false;\n";
            out << "        ops = val;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 604:
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0,0,0},{1,1,0},{0,0,0}};\n";
            out << "        if (spreadRot(g, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0,1},{1,0}};\n";
            out << "        if (spreadRot(g, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0}};\n";
            out << "        if (spreadRot(g, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0, 0}, {0, 0}};\n";
            out << "        if (spreadRot(g, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{1, 0}, {0, 0}};\n";
            out << "        if (spreadRot(g, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> g(n, vector<int>(n, 0));\n";
            out << "        if (spreadRot(g, 0) != 2 * (n - 1) + 1) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 605:
            out << "    {\n";
            out << "        int INF = 2147483647;\n";
            out << "        vector<vector<int>> g = {\n";
            out << "            {INF, -1,  0, INF},\n";
            out << "            {INF, INF, INF, -1},\n";
            out << "            {INF, -1, INF, -1},\n";
            out << "            { 0,  -1, INF, INF}\n";
            out << "        };\n";
            out << "        spreadRot(g, 0);\n";
            out << "        int sum = 0;\n";
            out << "        for (auto& row : g)\n";
            out << "            for (int v : row)\n";
            out << "                if (v > 0) sum += v;\n";
            out << "        if (sum != 19) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        int INF = 2147483647;\n";
            out << "        vector<vector<int>> g = {{0, INF, INF}};\n";
            out << "        spreadRot(g, 0);\n";
            out << "        if (g[0][1] != 1 || g[0][2] != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> g = {{0, -1, 0}};\n";
            out << "        spreadRot(g, 0);\n";
            out << "        if (g[0][1] != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        int INF = 2147483647;\n";
            out << "        vector<vector<int>> g = {{INF}};\n";
            out << "        spreadRot(g, 0);\n";
            out << "        if (g[0][0] != INF) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        int INF = 2147483647;\n";
            out << "        vector<vector<int>> g(n, vector<int>(n, INF));\n";
            out << "        g[0][0] = 0;\n";
            out << "        spreadRot(g, 0);\n";
            out << "        if (g[n-1][n-1] != 2 * (n - 1)) all_correct = false;\n";
            out << "        if (g[1][0] != 1) all_correct = false;\n";
            out << "        ops = n * n;\n";
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

    out << "const { spreadRot } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 601: // First Tendril
            out << "if (spreadRot([[1, 2], [3, 4]], 3) !== 6) allCorrect = false;\n";
            out << "if (spreadRot([[1, 2], [3, 4]], 99) !== 10) allCorrect = false;\n";
            out << "if (spreadRot([[0, 0], [0, 5]], 5) !== 5) allCorrect = false;\n";
            out << "if (spreadRot([[7]], 7) !== 7) allCorrect = false;\n";
            out << "if (spreadRot([[1, 2, 3], [4, 5, 6], [7, 8, 9]], 5) !== 15) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let g = [];\n";
            out << "    for (let i = 0; i < n; i++) g.push(new Array(n).fill(1));\n";
            out << "    if (spreadRot(g, 99) !== n * n) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 602: // Spread the Spores
            out << "if (spreadRot([[2,1,1],[1,1,0],[0,1,1]], 0) !== 4) allCorrect = false;\n";
            out << "if (spreadRot([[2,1,1],[0,1,1],[1,0,1]], 0) !== -1) allCorrect = false;\n";
            out << "if (spreadRot([[0, 2]], 0) !== 0) allCorrect = false;\n";
            out << "if (spreadRot([[2, 1, 1, 1, 1]], 0) !== 4) allCorrect = false;\n";
            out << "if (spreadRot([[2]], 0) !== 0) allCorrect = false;\n";
            out << "if (spreadRot([[1]], 0) !== -1) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let g = [];\n";
            out << "    for (let i = 0; i < n; i++) g.push(new Array(n).fill(1));\n";
            out << "    g[0][0] = 2;\n";
            out << "    if (spreadRot(g, 0) !== 2 * (n - 1)) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 603: // Consume Layer by Layer
            out << "if (spreadRot([[3], [9, 20], [-1, -1, 15, 7]], 2) !== 22) allCorrect = false;\n";
            out << "if (spreadRot([[1]], 0) !== 1) allCorrect = false;\n";
            out << "if (spreadRot([[1], [2, 3], [4, 5, 6, 7]], 1) !== 5) allCorrect = false;\n";
            out << "if (spreadRot([[1], [2, 3], [4, 5, 6, 7]], 0) !== 1) allCorrect = false;\n";
            out << "if (spreadRot([[1], [2, -1], [3, -1, -1, -1]], 1) !== 2) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let levels = 15;\n";
            out << "    let g = [];\n";
            out << "    let val = 1;\n";
            out << "    for (let l = 0; l < levels; l++) {\n";
            out << "        let count = 1 << l;\n";
            out << "        let row = [];\n";
            out << "        for (let i = 0; i < count; i++) row.push(val++);\n";
            out << "        g.push(row);\n";
            out << "    }\n";
            out << "    if (spreadRot(g, 0) !== 1) allCorrect = false;\n";
            out << "    if (spreadRot(g, 1) !== 5) allCorrect = false;\n";
            out << "    ops = val;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 604: // Shortest Path
            out << "if (spreadRot([[0,0,0],[1,1,0],[0,0,0]], 0) !== 5) allCorrect = false;\n";
            out << "if (spreadRot([[0,1],[1,0]], 0) !== -1) allCorrect = false;\n";
            out << "if (spreadRot([[0]], 0) !== 1) allCorrect = false;\n";
            out << "if (spreadRot([[0, 0], [0, 0]], 0) !== 3) allCorrect = false;\n";
            out << "if (spreadRot([[1, 0], [0, 0]], 0) !== -1) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let g = [];\n";
            out << "    for (let i = 0; i < n; i++) g.push(new Array(n).fill(0));\n";
            out << "    if (spreadRot(g, 0) !== 2 * (n - 1) + 1) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 605: // The Awakening (boss)
            out << "{\n";
            out << "    const INF = 2147483647;\n";
            out << "    let g = [\n";
            out << "        [INF, -1,  0, INF],\n";
            out << "        [INF, INF, INF, -1],\n";
            out << "        [INF, -1, INF, -1],\n";
            out << "        [ 0,  -1, INF, INF]\n";
            out << "    ];\n";
            out << "    spreadRot(g, 0);\n";
            out << "    let sum = 0;\n";
            out << "    for (let row of g)\n";
            out << "        for (let v of row)\n";
            out << "            if (v > 0) sum += v;\n";
            out << "    if (sum !== 19) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    const INF = 2147483647;\n";
            out << "    let g = [[0, INF, INF]];\n";
            out << "    spreadRot(g, 0);\n";
            out << "    if (g[0][1] !== 1 || g[0][2] !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let g = [[0, -1, 0]];\n";
            out << "    spreadRot(g, 0);\n";
            out << "    if (g[0][1] !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    const INF = 2147483647;\n";
            out << "    let g = [[INF]];\n";
            out << "    spreadRot(g, 0);\n";
            out << "    if (g[0][0] !== INF) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    const INF = 2147483647;\n";
            out << "    let g = [];\n";
            out << "    for (let i = 0; i < n; i++) g.push(new Array(n).fill(INF));\n";
            out << "    g[0][0] = 0;\n";
            out << "    spreadRot(g, 0);\n";
            out << "    if (g[n-1][n-1] !== 2 * (n - 1)) allCorrect = false;\n";
            out << "    if (g[1][0] !== 1) allCorrect = false;\n";
            out << "    ops = n * n;\n";
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
