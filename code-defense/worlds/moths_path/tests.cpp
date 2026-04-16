#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> moths_path::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1401, "Climbing the Lanterns", WORLD_NAME, WORLD_DESC,
        "How many distinct ways can the moth reach step target if it can jump 1 or 2 steps at a time? lanterns is unused.",
        "Example: target=2 -> 2 (1+1 or 2)\nExample: target=3 -> 3 (1+1+1, 1+2, 2+1)\nExample: target=1 -> 1",
        "1 <= target <= 45",
        45, 1000, 300,
        "int flutterForward(vector<int>& lanterns, int target)",
        "dp[i] = dp[i-1] + dp[i-2]. Base cases dp[0]=1, dp[1]=1. Each step can come from 1 or 2 steps back. O(n) time, O(1) space with two variables.", generateStub, generateRunner
    });

    waves.push_back({
        1402, "Maximum Warmth", WORLD_NAME, WORLD_DESC,
        "The moth cannot visit two adjacent lanterns. Return the maximum sum of lantern values it can collect (house robber). target is unused.",
        "Example: [1,2,3,1] -> 4 (1+3)\nExample: [2,7,9,3,1] -> 12 (2+9+1)\nExample: [5] -> 5",
        "1 <= N <= 100,000\nAll values >= 0.",
        100000, 1000, 300,
        "int flutterForward(vector<int>& lanterns, int target)",
        "dp[i] = max(dp[i-1], dp[i-2] + nums[i]). Either skip current (take previous best) or take current + best from two back. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1403, "Minimum Cost Path", WORLD_NAME, WORLD_DESC,
        "Each lantern costs lanterns[i] to land on. The moth can jump 1 or 2 steps. Find the minimum cost to reach past the end (index N). Start from index 0 or 1. target is unused.",
        "Example: [10,15,20] -> 15 (start at index 1, jump 2)\nExample: [1,100,1,1,1,100,1,1,100,1] -> 6\nExample: [0,0,0] -> 0",
        "2 <= N <= 100,000\nAll values >= 0.",
        100000, 1000, 300,
        "int flutterForward(vector<int>& lanterns, int target)",
        "dp[i] = cost[i] + min(dp[i-1], dp[i-2]). Start from step 0 or 1. Return min(dp[n-1], dp[n-2]). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1404, "Coin Lanterns", WORLD_NAME, WORLD_DESC,
        "lanterns holds denominations. Return the minimum number of lanterns (coins) needed to make exactly target. Return -1 if impossible.",
        "Example: [1,5,10], target=11 -> 3 (10+1)\nExample: [2], target=3 -> -1\nExample: [1], target=0 -> 0\nExample: [1,2,5], target=11 -> 3 (5+5+1)",
        "1 <= denominations <= 12\n0 <= target <= 10,000",
        10000, 1000, 300,
        "int flutterForward(vector<int>& lanterns, int target)",
        "dp[amount] = min(dp[amount - coin] + 1) for each coin. Initialize dp[0]=0, all others = infinity. O(amount * coins).", generateStub, generateRunner
    });

    waves.push_back({
        1405, "The Longest Glow", WORLD_NAME, WORLD_DESC,
        "Find the length of the longest strictly increasing subsequence of lantern values. target is unused. This is the boss wave.",
        "Example: [10,9,2,5,3,7,101,18] -> 4 (2,3,7,101)\nExample: [0,1,0,3,2,3] -> 4 (0,1,2,3)\nExample: [7,7,7] -> 1",
        "1 <= N <= 10,000",
        10000, 2000, 300,
        "int flutterForward(vector<int>& lanterns, int target)",
        "dp[i] = length of LIS ending at i. For each j < i, if nums[j] < nums[i], dp[i] = max(dp[i], dp[j]+1). O(n^2) basic, O(n log n) with binary search on tails array.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a dynamic-programming approach",
        "Need to reuse overlapping subproblems instead of recomputing them",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Dynamic programming over prefix endings",
            "Heap of current best subsequences",
            "Binary search on answer with greedy check",
            "Union-Find over increasing pairs"
        },
        0,
        "Need to reuse overlapping subproblems instead of recomputing them",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given the stair or lantern rules described by the wave, return the number of distinct ways to reach the end.";
    waves[0].flavor_text = "A lantern keeper watches moths spiral around the cliffside lights and counts the ways a careful climber might ascend through the glow without losing the path.";
    waves[1].clear_prompt = "Given the array and adjacency restriction described by the wave, return the maximum value obtainable without taking adjacent positions.";
    waves[1].flavor_text = "The warm lamps along the trail are tempting, but taking two neighbors trips the hidden line between them. The lantern keeper must choose which lights to claim and which to leave glowing alone.";
    waves[2].clear_prompt = "Given the cost array described by the wave, return the minimum total cost to reach the end under the allowed moves.";
    waves[2].flavor_text = "The moth path rises through ash and wind, and every step exacts a toll. The lantern keeper wants the cheapest ascent, learning from every partial climb already measured on the way up.";
    waves[3].clear_prompt = "Given coin values and a target amount as defined by the wave, return the minimum number of coins or number of combinations required.";
    waves[3].flavor_text = "The moths pay in tiny minted discs of light. The lantern keeper must decide how to combine them to reach the exact amount without repeating work already solved.";
    waves[4].clear_prompt = "Given the sequence described by the wave, return the length of the longest increasing subsequence or the equivalent 1D-DP answer.";
    waves[4].flavor_text = "Across the night trail, lights rise and fall like breathing. The lantern keeper searches for the longest glow that keeps climbing brighter without ever turning back.";
    return waves;
}

void moths_path::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <algorithm>\n";
        out << "#include <climits>\n";
        out << "using namespace std;\n\n";

        out << "int flutterForward(vector<int>& lanterns, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 1401: // Climbing the Lanterns
                out << "    {\n";
                out << "        vector<int> v = {};\n";
                out << "        if (flutterForward(v, 2) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {};\n";
                out << "        if (flutterForward(v, 3) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {};\n";
                out << "        if (flutterForward(v, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {};\n";
                out << "        if (flutterForward(v, 5) != 8) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<int> v = {};\n";
                out << "        if (flutterForward(v, 40) != 165580141) all_correct = false;\n";
                out << "        ops = 40;\n";
                out << "    } else { ops = 11; }\n";
                break;

            case 1402: // Maximum Warmth
                out << "    {\n";
                out << "        vector<int> v = {1, 2, 3, 1};\n";
                out << "        if (flutterForward(v, 0) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {2, 7, 9, 3, 1};\n";
                out << "        if (flutterForward(v, 0) != 12) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {5};\n";
                out << "        if (flutterForward(v, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {2, 1};\n";
                out << "        if (flutterForward(v, 0) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> v(n, 1);\n";
                out << "        int expected = (n + 1) / 2;\n";
                out << "        if (flutterForward(v, 0) != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 12; }\n";
                break;

            case 1403: // Minimum Cost Path
                out << "    {\n";
                out << "        vector<int> v = {10, 15, 20};\n";
                out << "        if (flutterForward(v, 0) != 15) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {1, 100, 1, 1, 1, 100, 1, 1, 100, 1};\n";
                out << "        if (flutterForward(v, 0) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {0, 0, 0};\n";
                out << "        if (flutterForward(v, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> v(n);\n";
                out << "        for (int i = 0; i < n; i++) v[i] = (i % 2 == 0) ? 1 : 100;\n";
                out << "        // Start at index 0 (cost 1), jump 2 each time, landing on even indices\n";
                out << "        // Number of even indices = ceil(n/2), cost = ceil(n/2)\n";
                out << "        // But we can also start at index 1 — that would cost more (100s)\n";
                out << "        // Starting at 0: land on 0,2,4,... costs are all 1s\n";
                out << "        // n=100000: even indices 0..99998, count=50000, cost=50000\n";
                out << "        // Actually min cost path can start at 0 or 1, so starting at 0 and jumping by 2\n";
                out << "        int expected = n / 2;\n";
                out << "        int r = flutterForward(v, 0);\n";
                out << "        if (r != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 1404: // Coin Lanterns
                out << "    {\n";
                out << "        vector<int> v = {1, 5, 10};\n";
                out << "        if (flutterForward(v, 11) != 3) all_correct = false; // 10+1\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {2};\n";
                out << "        if (flutterForward(v, 3) != -1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {1};\n";
                out << "        if (flutterForward(v, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {1, 2, 5};\n";
                out << "        if (flutterForward(v, 11) != 3) all_correct = false; // 5+5+1\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        vector<int> v = {1, 5, 10, 25};\n";
                out << "        if (flutterForward(v, 999) != 45) all_correct = false; // 39*25+2*10+4*1\n";
                out << "        ops = 999;\n";
                out << "    } else { ops = 15; }\n";
                break;

            case 1405: // The Longest Glow (boss)
                out << "    {\n";
                out << "        vector<int> v = {10, 9, 2, 5, 3, 7, 101, 18};\n";
                out << "        if (flutterForward(v, 0) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {0, 1, 0, 3, 2, 3};\n";
                out << "        if (flutterForward(v, 0) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {7, 7, 7};\n";
                out << "        if (flutterForward(v, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> v = {1};\n";
                out << "        if (flutterForward(v, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> v(n);\n";
                out << "        // Pseudo-random pattern: sawtooth with varying peaks\n";
                out << "        for (int i = 0; i < n; i++) v[i] = (i * 7 + 3) % 1000;\n";
                out << "        int r = flutterForward(v, 0);\n";
                out << "        if (r <= 0) all_correct = false;\n";
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

        out << "const { flutterForward } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 1401: // Climbing the Lanterns
                out << "{\n";
                out << "    let l = [];\n";
                out << "    if (flutterForward(l, 2) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [];\n";
                out << "    if (flutterForward(l, 3) !== 3) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [];\n";
                out << "    if (flutterForward(l, 1) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [];\n";
                out << "    if (flutterForward(l, 5) !== 8) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let l = [];\n";
                out << "    if (flutterForward(l, 40) !== 165580141) allCorrect = false;\n";
                out << "    ops = 40;\n";
                out << "} else { ops = 11; }\n";
                break;

            case 1402: // Maximum Warmth
                out << "{\n";
                out << "    let l = [1, 2, 3, 1];\n";
                out << "    if (flutterForward(l, 0) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [2, 7, 9, 3, 1];\n";
                out << "    if (flutterForward(l, 0) !== 12) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [5];\n";
                out << "    if (flutterForward(l, 0) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [2, 1];\n";
                out << "    if (flutterForward(l, 0) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let l = new Array(n).fill(1);\n";
                out << "    let expected = Math.ceil(n / 2);\n";
                out << "    if (flutterForward(l, 0) !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 12; }\n";
                break;

            case 1403: // Minimum Cost Path
                out << "{\n";
                out << "    let l = [10, 15, 20];\n";
                out << "    if (flutterForward(l, 0) !== 15) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [1, 100, 1, 1, 1, 100, 1, 1, 100, 1];\n";
                out << "    if (flutterForward(l, 0) !== 6) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [0, 0, 0];\n";
                out << "    if (flutterForward(l, 0) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let l = new Array(n);\n";
                out << "    for (let i = 0; i < n; i++) l[i] = (i % 2 === 0) ? 1 : 100;\n";
                out << "    let expected = Math.floor(n / 2);\n";
                out << "    let r = flutterForward(l, 0);\n";
                out << "    if (r !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 1404: // Coin Lanterns
                out << "{\n";
                out << "    let l = [1, 5, 10];\n";
                out << "    if (flutterForward(l, 11) !== 3) allCorrect = false; // 10+1\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [2];\n";
                out << "    if (flutterForward(l, 3) !== -1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [1];\n";
                out << "    if (flutterForward(l, 0) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [1, 2, 5];\n";
                out << "    if (flutterForward(l, 11) !== 3) allCorrect = false; // 5+5+1\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let l = [1, 5, 10, 25];\n";
                out << "    if (flutterForward(l, 999) !== 45) allCorrect = false; // 39*25+2*10+4*1\n";
                out << "    ops = 999;\n";
                out << "} else { ops = 15; }\n";
                break;

            case 1405: // The Longest Glow (boss)
                out << "{\n";
                out << "    let l = [10, 9, 2, 5, 3, 7, 101, 18];\n";
                out << "    if (flutterForward(l, 0) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [0, 1, 0, 3, 2, 3];\n";
                out << "    if (flutterForward(l, 0) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [7, 7, 7];\n";
                out << "    if (flutterForward(l, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let l = [1];\n";
                out << "    if (flutterForward(l, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let l = new Array(n);\n";
                out << "    for (let i = 0; i < n; i++) l[i] = (i * 7 + 3) % 1000;\n";
                out << "    let r = flutterForward(l, 0);\n";
                out << "    if (r <= 0) allCorrect = false;\n";
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
