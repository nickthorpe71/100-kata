#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> silk_loom::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1501, "Count the Weaves", WORLD_NAME, WORLD_DESC,
        "Count unique paths from top-left to bottom-right. You may only move right or down. Grid dimensions come from fabric size. target is unused.",
        "Example: [[0,0,0],[0,0,0]] (2x3) -> 3\nExample: [[0,0],[0,0],[0,0]] (3x2) -> 3\nExample: [[0]] (1x1) -> 1",
        "1 <= rows, cols <= 15",
        15, 1000, 300,
        "int weaveLoom(vector<vector<int>>& fabric, int target)",
        "dp[r][c] = dp[r-1][c] + dp[r][c-1]. First row and column are all 1s. Each cell is the sum of paths from above and left. O(rows*cols).", generateStub, generateRunner
    });

    waves.push_back({
        1502, "Minimum Thread", WORLD_NAME, WORLD_DESC,
        "Find the minimum path sum from top-left to bottom-right. You may only move right or down. target is unused.",
        "Example: [[1,3,1],[1,5,1],[4,2,1]] -> 7 (1+3+1+1+1)\nExample: [[1,2],[3,4]] -> 7 (1+2+4)\nExample: [[5]] -> 5",
        "1 <= rows, cols <= 500\n1 <= fabric[i][j] <= 1000",
        500, 1000, 300,
        "int weaveLoom(vector<vector<int>>& fabric, int target)",
        "dp[r][c] = grid[r][c] + min(dp[r-1][c], dp[r][c-1]). Initialize first row and column as cumulative sums. O(rows*cols).", generateStub, generateRunner
    });

    waves.push_back({
        1503, "Match the Pattern", WORLD_NAME, WORLD_DESC,
        "Edit distance. fabric[0] contains ASCII values of string1, fabric[1] contains ASCII values of string2. Return the minimum number of single-character edits (insert, delete, replace) to transform string1 into string2. target is unused.",
        "Example: \"horse\"/\"ros\" -> fabric=[[104,111,114,115,101],[114,111,115]] -> 3\nExample: \"abc\"/\"abc\" -> 0\nExample: \"\"/\"abc\" -> fabric=[[],[97,98,99]] -> 3",
        "0 <= len(string1), len(string2) <= 1000",
        1000, 2000, 300,
        "int weaveLoom(vector<vector<int>>& fabric, int target)",
        "Edit distance DP. dp[i][j] = 0 if chars match (take dp[i-1][j-1]), otherwise 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1]) for insert/delete/replace. O(n*m).", generateStub, generateRunner
    });

    waves.push_back({
        1504, "Pack the Loom", WORLD_NAME, WORLD_DESC,
        "0/1 knapsack. fabric[i] = {weight, value} for each item. target = capacity. Return the maximum total value that fits within the capacity.",
        "Example: [[1,6],[2,10],[3,12]], target=5 -> 22 (items 0+2: weight 4, value 18? No: all three: weight 6 > 5. Items 1+2: weight 5, value 22)\nExample: [[1,1]], target=0 -> 0\nExample: [[5,10]], target=3 -> 0",
        "1 <= items <= 100\n1 <= weight, value <= 1000\n0 <= capacity <= 1000",
        100, 2000, 300,
        "int weaveLoom(vector<vector<int>>& fabric, int target)",
        "dp[i][w] = max(dp[i-1][w], dp[i-1][w-weight[i]] + value[i]) if weight fits. Iterate items, then capacities. Can optimize to 1D array iterating capacity backwards. O(n*capacity).", generateStub, generateRunner
    });

    waves.push_back({
        1505, "The Longest Weave", WORLD_NAME, WORLD_DESC,
        "Longest common subsequence. fabric[0] contains ASCII values of string1, fabric[1] contains ASCII values of string2. Return the length of the longest common subsequence. target is unused.",
        "Example: \"abcde\"/\"ace\" -> fabric=[[97,98,99,100,101],[97,99,101]] -> 3\nExample: \"abc\"/\"def\" -> 0\nExample: \"abc\"/\"abc\" -> 3",
        "0 <= len(string1), len(string2) <= 500",
        500, 2000, 300,
        "int weaveLoom(vector<vector<int>>& fabric, int target)",
        "dp[i][j] = dp[i-1][j-1]+1 if chars match, else max(dp[i-1][j], dp[i][j-1]). Build table bottom-up. O(n*m).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a 2D dynamic-programming approach",
        "Need to combine answers from neighboring subproblems in a grid or table state",
        "O(n*m) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "2D dynamic programming table over both strings",
            "Sliding window over matching runs",
            "Union-Find over equal characters",
            "Heap of candidate subsequences"
        },
        0,
        "Need to combine answers from neighboring subproblems in a grid or table state",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n*m) space")
    );
    waves[0].clear_prompt = "Given a grid `fabric`, return the number of unique paths from the top-left to the bottom-right when movement is restricted to right and down.";
    waves[0].flavor_text = "The master weaver studies a loom where every intersection offers only two honest moves: right or down. She counts all possible weaves that reach the far corner without tearing the pattern.";
    waves[1].clear_prompt = "Given a grid `fabric`, return the minimum path sum from the top-left to the bottom-right when movement is restricted to right and down.";
    waves[1].flavor_text = "Not all routes across the loom cost the same. The master weaver glides over taut and slack strands alike, searching for the cheapest path through the fabric.";
    waves[2].clear_prompt = "Given two strings encoded as ASCII rows in `fabric`, return their edit distance.";
    waves[2].flavor_text = "Two woven phrases almost match, but not quite. The master weaver counts the smallest set of cuts, insertions, and substitutions needed to make one pattern become the other.";
    waves[3].clear_prompt = "Given items in `fabric` as `{weight, value}` pairs and capacity `target`, return the maximum value achievable without exceeding the capacity.";
    waves[3].flavor_text = "The loom can bear only so much before the frame warps. The master weaver chooses which weighted bundles of thread to carry, trying to maximize value without breaking the rig.";
    waves[4].clear_prompt = "Given two strings encoded as ASCII rows in `fabric`, return the length of their longest common subsequence.";
    waves[4].flavor_text = "Across two old tapestries, the master weaver traces a pattern that survives in both even when gaps and damage interrupt the line. She seeks the longest sequence still shared by both works.";
    return waves;
}

void silk_loom::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int weaveLoom(vector<vector<int>>& fabric, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1501: // Count the Weaves
            out << "    {\n";
            out << "        vector<vector<int>> f = {{0,0,0},{0,0,0}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{0,0},{0,0},{0,0}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{0}};\n";
            out << "        if (weaveLoom(f, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{0,0,0,0},{0,0,0,0},{0,0,0,0}};\n";
            out << "        if (weaveLoom(f, 0) != 10) all_correct = false; // C(5,2)\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{0,0}};\n";
            out << "        if (weaveLoom(f, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> f(n, vector<int>(n, 0));\n";
            out << "        // 15x15 grid: C(28,14) = 40116600\n";
            out << "        if (weaveLoom(f, 0) != 40116600) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 1502: // Minimum Thread
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,3,1},{1,5,1},{4,2,1}};\n";
            out << "        if (weaveLoom(f, 0) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,2},{3,4}};\n";
            out << "        if (weaveLoom(f, 0) != 7) all_correct = false; // 1+2+4\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{5}};\n";
            out << "        if (weaveLoom(f, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,2,3},{4,5,6}};\n";
            out << "        if (weaveLoom(f, 0) != 12) all_correct = false; // 1+2+3+6\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,10,1},{1,10,1},{1,1,1}};\n";
            out << "        if (weaveLoom(f, 0) != 5) all_correct = false; // 1+1+1+1+1\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> f(n, vector<int>(n, 1));\n";
            out << "        // All 1s: min path = 2*(n-1)+1 = 2*n-1 = 999\n";
            out << "        if (weaveLoom(f, 0) != 2 * n - 1) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 1503: // Match the Pattern (edit distance)
            out << "    {\n";
            out << "        // \"horse\" / \"ros\"\n";
            out << "        vector<vector<int>> f = {{104,111,114,115,101},{114,111,115}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"abc\" / \"abc\"\n";
            out << "        vector<vector<int>> f = {{97,98,99},{97,98,99}};\n";
            out << "        if (weaveLoom(f, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"\" / \"abc\"\n";
            out << "        vector<vector<int>> f = {{},{97,98,99}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"intention\" / \"execution\"\n";
            out << "        vector<vector<int>> f = {{105,110,116,101,110,116,105,111,110},{101,120,101,99,117,116,105,111,110}};\n";
            out << "        if (weaveLoom(f, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"a\" / \"b\"\n";
            out << "        vector<vector<int>> f = {{97},{98}};\n";
            out << "        if (weaveLoom(f, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        // Two identical strings of length n -> edit distance 0\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = 97 + (i % 26);\n";
            out << "        vector<vector<int>> f = {s, s};\n";
            out << "        if (weaveLoom(f, 0) != 0) all_correct = false;\n";
            out << "        // Two completely different strings of length n -> edit distance n\n";
            out << "        vector<int> s2(n, 65);\n";  // all 'A'
            out << "        vector<int> s3(n, 90);\n";  // all 'Z'
            out << "        vector<vector<int>> f2 = {s2, s3};\n";
            out << "        if (weaveLoom(f2, 0) != n) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 20; }\n";
            break;

        case 1504: // Pack the Loom (0/1 knapsack)
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,6},{2,10},{3,12}};\n";
            out << "        if (weaveLoom(f, 5) != 22) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{1,1}};\n";
            out << "        if (weaveLoom(f, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{5,10}};\n";
            out << "        if (weaveLoom(f, 3) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{2,3},{3,4},{4,5},{5,6}};\n";
            out << "        if (weaveLoom(f, 8) != 10) all_correct = false; // items 1+2: w=7, v=9? items 0+2: w=6,v=8. items 0+1+2: w=9>8. items 1+3: w=8, v=10\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> f = {{10,100},{20,200},{30,300}};\n";
            out << "        if (weaveLoom(f, 50) != 500) all_correct = false; // items 1+2: w=50, v=500\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        int cap = 1000;\n";
            out << "        // 100 items, each weight=10, value=10. Capacity 1000 -> take all 100 items = 1000\n";
            out << "        vector<vector<int>> f(n, vector<int>{10, 10});\n";
            out << "        if (weaveLoom(f, cap) != n * 10) all_correct = false;\n";
            out << "        ops = n * cap;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 1505: // The Longest Weave (boss) - LCS
            out << "    {\n";
            out << "        // \"abcde\" / \"ace\"\n";
            out << "        vector<vector<int>> f = {{97,98,99,100,101},{97,99,101}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"abc\" / \"def\"\n";
            out << "        vector<vector<int>> f = {{97,98,99},{100,101,102}};\n";
            out << "        if (weaveLoom(f, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"abc\" / \"abc\"\n";
            out << "        vector<vector<int>> f = {{97,98,99},{97,98,99}};\n";
            out << "        if (weaveLoom(f, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"abcbdab\" / \"bdcaba\"\n";
            out << "        vector<vector<int>> f = {{97,98,99,98,100,97,98},{98,100,99,97,98,97}};\n";
            out << "        if (weaveLoom(f, 0) != 4) all_correct = false; // \"bcba\" or \"bdab\"\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // \"a\" / \"a\"\n";
            out << "        vector<vector<int>> f = {{97},{97}};\n";
            out << "        if (weaveLoom(f, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        // Two identical strings of length n -> LCS = n\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = 97 + (i % 26);\n";
            out << "        vector<vector<int>> f = {s, s};\n";
            out << "        if (weaveLoom(f, 0) != n) all_correct = false;\n";
            out << "        ops = n * n;\n";
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

    out << "const { weaveLoom } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1501: // Count the Weaves
            out << "{\n";
            out << "    let f = [[0,0,0],[0,0,0]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[0,0],[0,0],[0,0]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[0]];\n";
            out << "    if (weaveLoom(f, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[0,0,0,0],[0,0,0,0],[0,0,0,0]];\n";
            out << "    if (weaveLoom(f, 0) !== 10) allCorrect = false; // C(5,2)\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[0,0]];\n";
            out << "    if (weaveLoom(f, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let f = Array.from({length: n}, () => new Array(n).fill(0));\n";
            out << "    if (weaveLoom(f, 0) !== 40116600) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 1502: // Minimum Thread
            out << "{\n";
            out << "    let f = [[1,3,1],[1,5,1],[4,2,1]];\n";
            out << "    if (weaveLoom(f, 0) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[1,2],[3,4]];\n";
            out << "    if (weaveLoom(f, 0) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[5]];\n";
            out << "    if (weaveLoom(f, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[1,2,3],[4,5,6]];\n";
            out << "    if (weaveLoom(f, 0) !== 12) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[1,10,1],[1,10,1],[1,1,1]];\n";
            out << "    if (weaveLoom(f, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let f = Array.from({length: n}, () => new Array(n).fill(1));\n";
            out << "    if (weaveLoom(f, 0) !== 2 * n - 1) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 1503: // Match the Pattern (edit distance)
            out << "{\n";
            out << "    let f = [[104,111,114,115,101],[114,111,115]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97,98,99],[97,98,99]];\n";
            out << "    if (weaveLoom(f, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[],[97,98,99]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[105,110,116,101,110,116,105,111,110],[101,120,101,99,117,116,105,111,110]];\n";
            out << "    if (weaveLoom(f, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97],[98]];\n";
            out << "    if (weaveLoom(f, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(97 + (i % 26));\n";
            out << "    let f = [s.slice(), s.slice()];\n";
            out << "    if (weaveLoom(f, 0) !== 0) allCorrect = false;\n";
            out << "    let s2 = new Array(n).fill(65);\n";
            out << "    let s3 = new Array(n).fill(90);\n";
            out << "    let f2 = [s2, s3];\n";
            out << "    if (weaveLoom(f2, 0) !== n) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 20; }\n";
            break;

        case 1504: // Pack the Loom (0/1 knapsack)
            out << "{\n";
            out << "    let f = [[1,6],[2,10],[3,12]];\n";
            out << "    if (weaveLoom(f, 5) !== 22) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[1,1]];\n";
            out << "    if (weaveLoom(f, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[5,10]];\n";
            out << "    if (weaveLoom(f, 3) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[2,3],[3,4],[4,5],[5,6]];\n";
            out << "    if (weaveLoom(f, 8) !== 10) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[10,100],[20,200],[30,300]];\n";
            out << "    if (weaveLoom(f, 50) !== 500) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let cap = 1000;\n";
            out << "    let f = Array.from({length: n}, () => [10, 10]);\n";
            out << "    if (weaveLoom(f, cap) !== n * 10) allCorrect = false;\n";
            out << "    ops = n * cap;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 1505: // The Longest Weave (boss) - LCS
            out << "{\n";
            out << "    let f = [[97,98,99,100,101],[97,99,101]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97,98,99],[100,101,102]];\n";
            out << "    if (weaveLoom(f, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97,98,99],[97,98,99]];\n";
            out << "    if (weaveLoom(f, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97,98,99,98,100,97,98],[98,100,99,97,98,97]];\n";
            out << "    if (weaveLoom(f, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let f = [[97],[97]];\n";
            out << "    if (weaveLoom(f, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(97 + (i % 26));\n";
            out << "    let f = [s.slice(), s.slice()];\n";
            out << "    if (weaveLoom(f, 0) !== n) allCorrect = false;\n";
            out << "    ops = n * n;\n";
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
