#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> island_post::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1301, "Count the Islands", WORLD_NAME, WORLD_DESC,
        "Count the number of connected components in an undirected graph given as an adjacency list. target is unused.",
        "Example: [[1],[0],[3],[2]] -> 2\nExample: [[]] -> 1\nExample: [[1,2],[0],[0]] -> 1\nExample: [[],[],[]] -> 3",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "DFS or BFS from each unvisited node. Each traversal marks a component. Count the number of traversals needed. O(V+E).", generateStub, generateRunner
    });

    waves.push_back({
        1302, "Can You Reach?", WORLD_NAME, WORLD_DESC,
        "Return 1 if node target is reachable from node 0, 0 otherwise.",
        "Example: [[1],[0,2],[1]], target=2 -> 1\nExample: [[1],[0],[]], target=2 -> 0\nExample: [[1,2],[0],[0]], target=1 -> 1",
        "1 <= N <= 100,000\n0 <= target < N",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "BFS or DFS from node 0. If target is visited during traversal, return 1. O(V+E).", generateStub, generateRunner
    });

    waves.push_back({
        1303, "Shortest Delivery", WORLD_NAME, WORLD_DESC,
        "Find the shortest path (number of edges) from node 0 to node target. Return -1 if unreachable.",
        "Example: [[1,2],[0,3],[0,3],[1,2]], target=3 -> 2\nExample: [[1],[0]], target=1 -> 1\nExample: [[],[]], target=1 -> -1",
        "1 <= N <= 100,000\n0 <= target < N",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "BFS from node 0. BFS guarantees shortest path in unweighted graphs. Track distance array. Return distance[target]. O(V+E).", generateStub, generateRunner
    });

    waves.push_back({
        1304, "Detect the Loop", WORLD_NAME, WORLD_DESC,
        "Return 1 if the undirected graph contains a cycle, 0 otherwise. target is unused.",
        "Example: [[1],[0,2],[1,0]] -> 1 (triangle)\nExample: [[1],[0,2],[1]] -> 0 (chain)\nExample: [[1],[0],[3],[2]] -> 0 (two separate edges)",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "DFS with three states: unvisited, in-progress, done. If you visit an in-progress node, cycle found. For undirected: track parent during DFS, if you reach a visited non-parent, cycle found. O(V+E).", generateStub, generateRunner
    });

    waves.push_back({
        1305, "Delivery Order", WORLD_NAME, WORLD_DESC,
        "Treat bridges as a DIRECTED adjacency list. Return 1 if a valid topological order exists (the graph is a DAG), 0 if a cycle exists. target is unused.",
        "Example: [[1],[2],[]] -> 1 (DAG 0->1->2)\nExample: [[1],[2],[0]] -> 0 (cycle)\nExample: [[1,2],[],[]] -> 1",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "Kahn's algorithm: compute in-degrees, enqueue nodes with in-degree 0, process and decrement neighbors' in-degrees. If all nodes processed, it's a DAG. O(V+E).", generateStub, generateRunner
    });

    waves.push_back({
        1306, "All Routes (boss)", WORLD_NAME, WORLD_DESC,
        "Count all distinct paths from node 0 to node target in a DAG (directed acyclic graph). bridges is a directed adjacency list.",
        "Example: [[1,2],[3],[3],[]], target=3 -> 2\nExample: [[1],[]], target=1 -> 1\nExample: [[1,2],[2,3],[3],[]], target=3 -> 3",
        "1 <= N <= 100,000\nGraph is a DAG.",
        100000, 1000, 300,
        "int deliverMail(vector<vector<int>>& bridges, int target)",
        "DFS with backtracking. From node 0, try each neighbor. Count paths that reach target. Mark/unmark visited to allow different paths. O(2^V) worst case in a DAG.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a graph traversal approach",
        "Need to explore nodes and edges while tracking visited state",
        "O(V) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "DFS with memoized path counts in a DAG",
            "Sliding window over node ids",
            "Union-Find over directed edges",
            "Heap of outgoing choices"
        },
        0,
        "Need to explore nodes and edges while tracking visited state",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(V) space")
    );
    waves[0].clear_prompt = "Given an undirected graph as an adjacency list `bridges`, return the number of connected components.";
    waves[0].flavor_text = "Across a broken archipelago, the island postmaster maps which islands still belong to the same web of bridges. Each disconnected cluster is its own lonely territory of rope, gulls, and stone.";
    waves[1].clear_prompt = "Given an undirected graph as an adjacency list `bridges`, return `1` if `target` is reachable from node `0` and `0` otherwise.";
    waves[1].flavor_text = "The postmaster has one urgent message to send from the first island and a hundred ways to fail. She follows bridge after bridge through the fog, asking whether the destination still lies inside the reachable world.";
    waves[2].clear_prompt = "Given an unweighted graph as an adjacency list `bridges`, return the shortest path length from node `0` to node `target`, or `-1` if unreachable.";
    waves[2].flavor_text = "Storm bells ring over the harbor. The postmaster needs the quickest delivery route, not just any route, and every unnecessary crossing may cost a life on the far shore.";
    waves[3].clear_prompt = "Given an undirected graph as an adjacency list `bridges`, return `1` if the graph contains a cycle and `0` otherwise.";
    waves[3].flavor_text = "Some routes on the island map fold back into themselves like old curses. The postmaster must discover whether the bridges form a loop before the couriers are sent to wander forever.";
    waves[4].clear_prompt = "Given a directed graph as an adjacency list `bridges`, return `1` if a valid topological ordering exists and `0` if the graph contains a cycle.";
    waves[4].flavor_text = "Orders from the island registry must be obeyed in sequence, but bad command chains circle back on themselves. The postmaster tests whether the deliveries can be arranged without contradiction.";
    waves[5].clear_prompt = "Given a directed acyclic graph as an adjacency list `bridges`, return the number of distinct paths from node `0` to node `target`.";
    waves[5].flavor_text = "When the sea begins to take bridges, one road is never enough. The postmaster counts every valid route from the main post to the target island, building resilience out of branching paths.";
    return waves;
}

void island_post::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <queue>\n";
        out << "#include <algorithm>\n";
        out << "using namespace std;\n\n";

        out << "int deliverMail(vector<vector<int>>& bridges, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 1301:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0},{3},{2}};\n";
                out << "        if (deliverMail(b, 0) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{}};\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{0},{0}};\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{},{},{}};\n";
                out << "        if (deliverMail(b, 0) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<vector<int>> b(n);\n";
                out << "        if (deliverMail(b, 0) != n) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 10; }\n";
                break;

            case 1302:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0,2},{1}};\n";
                out << "        if (deliverMail(b, 2) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0},{}};\n";
                out << "        if (deliverMail(b, 2) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{0},{0}};\n";
                out << "        if (deliverMail(b, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<vector<int>> b(n);\n";
                out << "        for (int i = 0; i < n - 1; i++) {\n";
                out << "            b[i].push_back(i + 1);\n";
                out << "            b[i + 1].push_back(i);\n";
                out << "        }\n";
                out << "        if (deliverMail(b, n - 1) != 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 9; }\n";
                break;

            case 1303:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{0,3},{0,3},{1,2}};\n";
                out << "        if (deliverMail(b, 3) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0}};\n";
                out << "        if (deliverMail(b, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{},{}};\n";
                out << "        if (deliverMail(b, 1) != -1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<vector<int>> b(n);\n";
                out << "        for (int i = 0; i < n - 1; i++) {\n";
                out << "            b[i].push_back(i + 1);\n";
                out << "            b[i + 1].push_back(i);\n";
                out << "        }\n";
                out << "        if (deliverMail(b, n - 1) != n - 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 9; }\n";
                break;

            case 1304:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0,2},{1,0}};\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0,2},{1}};\n";
                out << "        if (deliverMail(b, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{0},{3},{2}};\n";
                out << "        if (deliverMail(b, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<vector<int>> b(n);\n";
                out << "        for (int i = 0; i < n - 1; i++) {\n";
                out << "            b[i].push_back(i + 1);\n";
                out << "            b[i + 1].push_back(i);\n";
                out << "        }\n";
                out << "        if (deliverMail(b, 0) != 0) all_correct = false;\n";
                out << "        b[n - 1].push_back(0);\n";
                out << "        b[0].push_back(n - 1);\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 10; }\n";
                break;

            case 1305:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{2},{}};\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{2},{0}};\n";
                out << "        if (deliverMail(b, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{},{}};\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<vector<int>> b(n);\n";
                out << "        for (int i = 0; i < n - 1; i++) {\n";
                out << "            b[i].push_back(i + 1);\n";
                out << "        }\n";
                out << "        if (deliverMail(b, 0) != 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 9; }\n";
                break;

            case 1306:
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{3},{3},{}};\n";
                out << "        if (deliverMail(b, 3) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1},{}};\n";
                out << "        if (deliverMail(b, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<vector<int>> b = {{1,2},{2,3},{3},{}};\n";
                out << "        if (deliverMail(b, 3) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<vector<int>> b = {{1,2},{2,3},{3},{}};\n";
                out << "        if (deliverMail(b, 3) != 3) all_correct = false;\n";
                out << "        ops = 4;\n";
                out << "    } else { ops = 9; }\n";
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

        out << "const { deliverMail } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 1301:
                out << "if (deliverMail([[1],[0],[3],[2]], 0) !== 2) allCorrect = false;\n";
                out << "if (deliverMail([[]], 0) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[1,2],[0],[0]], 0) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[],[],[]], 0) !== 3) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const b = [];\n";
                out << "    for (let i = 0; i < n; i++) b.push([]);\n";
                out << "    if (deliverMail(b, 0) !== n) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 10; }\n";
                break;

            case 1302:
                out << "if (deliverMail([[1],[0,2],[1]], 2) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[1],[0],[]], 2) !== 0) allCorrect = false;\n";
                out << "if (deliverMail([[1,2],[0],[0]], 1) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const b = [];\n";
                out << "    for (let i = 0; i < n; i++) b.push([]);\n";
                out << "    for (let i = 0; i < n - 1; i++) {\n";
                out << "        b[i].push(i + 1);\n";
                out << "        b[i + 1].push(i);\n";
                out << "    }\n";
                out << "    if (deliverMail(b, n - 1) !== 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 9; }\n";
                break;

            case 1303:
                out << "if (deliverMail([[1,2],[0,3],[0,3],[1,2]], 3) !== 2) allCorrect = false;\n";
                out << "if (deliverMail([[1],[0]], 1) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[],[]], 1) !== -1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const b = [];\n";
                out << "    for (let i = 0; i < n; i++) b.push([]);\n";
                out << "    for (let i = 0; i < n - 1; i++) {\n";
                out << "        b[i].push(i + 1);\n";
                out << "        b[i + 1].push(i);\n";
                out << "    }\n";
                out << "    if (deliverMail(b, n - 1) !== n - 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 9; }\n";
                break;

            case 1304:
                out << "if (deliverMail([[1],[0,2],[1,0]], 0) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[1],[0,2],[1]], 0) !== 0) allCorrect = false;\n";
                out << "if (deliverMail([[1],[0],[3],[2]], 0) !== 0) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const b = [];\n";
                out << "    for (let i = 0; i < n; i++) b.push([]);\n";
                out << "    for (let i = 0; i < n - 1; i++) {\n";
                out << "        b[i].push(i + 1);\n";
                out << "        b[i + 1].push(i);\n";
                out << "    }\n";
                out << "    if (deliverMail(b, 0) !== 0) allCorrect = false;\n";
                out << "    b[n - 1].push(0);\n";
                out << "    b[0].push(n - 1);\n";
                out << "    if (deliverMail(b, 0) !== 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 10; }\n";
                break;

            case 1305:
                out << "if (deliverMail([[1],[2],[]], 0) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[1],[2],[0]], 0) !== 0) allCorrect = false;\n";
                out << "if (deliverMail([[1,2],[],[]], 0) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const b = [];\n";
                out << "    for (let i = 0; i < n; i++) b.push([]);\n";
                out << "    for (let i = 0; i < n - 1; i++) {\n";
                out << "        b[i].push(i + 1);\n";
                out << "    }\n";
                out << "    if (deliverMail(b, 0) !== 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 9; }\n";
                break;

            case 1306:
                out << "if (deliverMail([[1,2],[3],[3],[]], 3) !== 2) allCorrect = false;\n";
                out << "if (deliverMail([[1],[]], 1) !== 1) allCorrect = false;\n";
                out << "if (deliverMail([[1,2],[2,3],[3],[]], 3) !== 3) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    if (deliverMail([[1,2],[2,3],[3],[]], 3) !== 3) allCorrect = false;\n";
                out << "    ops = 4;\n";
                out << "} else { ops = 9; }\n";
                break;
        }

        out << "\nconst end = process.hrtime.bigint();\n";
        out << "const ms = Number((end - start) / 1000000n);\n";
        out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
        out << "process.exit(allCorrect ? 0 : 1);\n";

        out.close();
    }
}
