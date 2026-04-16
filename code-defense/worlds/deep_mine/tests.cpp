#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> deep_mine::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2501, "Split the Load", WORLD_NAME, WORLD_DESC,
        "Split shaft into target contiguous groups to minimize the maximum group sum. Return that minimum possible maximum sum. Binary search on the answer, greedy validation.",
        "Example: [7,2,5,10,8], target=2 -> 18 ([7,2,5] and [10,8])\nExample: [1,2,3,4,5], target=3 -> 6\nExample: [1], target=1 -> 1",
        "1 <= N <= 100,000\n1 <= target <= N\nAll elements >= 1.",
        100000, 1000, 300,
        "int digDeep(vector<int>& shaft, int target)",
        "Binary search on the answer (max group sum). For a candidate max, greedily assign elements to groups -- start new group when adding next element would exceed max. If groups <= target, candidate is feasible. O(n log sum).", generateStub, generateRunner
    });

    waves.push_back({
        2502, "Mining Speed", WORLD_NAME, WORLD_DESC,
        "Koko eating bananas variant: shaft[i] = ore in section i. Miner processes at speed k per hour (ceiling division). Find minimum speed to finish ALL sections in target hours. Return that speed.",
        "Example: [3,6,7,11], target=8 -> 4\nExample: [30,11,23,4,20], target=5 -> 30\nExample: [30,11,23,4,20], target=6 -> 23\nExample: [1], target=1 -> 1",
        "1 <= N <= 100,000\n1 <= target >= N\nshaft[i] >= 1.",
        100000, 1000, 300,
        "int digDeep(vector<int>& shaft, int target)",
        "Binary search on speed k. For each candidate speed, check if total hours (sum of ceil(shaft[i]/k)) <= target. O(n log max).", generateStub, generateRunner
    });

    waves.push_back({
        2503, "Shipping Capacity", WORLD_NAME, WORLD_DESC,
        "shaft[i] = package weight. Ship them in order within target days. Find minimum ship capacity. Return that capacity.",
        "Example: [1,2,3,4,5,6,7,8,9,10], target=5 -> 15\nExample: [3,2,2,4,1,4], target=3 -> 6\nExample: [1,2,3,1,1], target=4 -> 3",
        "1 <= N <= 100,000\n1 <= target <= N\nshaft[i] >= 1.",
        100000, 1000, 300,
        "int digDeep(vector<int>& shaft, int target)",
        "Binary search on capacity. For each candidate, greedily fill days -- start new day when adding next package exceeds capacity. If days <= target, feasible. O(n log sum).", generateStub, generateRunner
    });

    waves.push_back({
        2504, "The Placement", WORLD_NAME, WORLD_DESC,
        "Place target markers along the shaft (shaft[i] = possible positions, sorted). Maximize the minimum distance between any two markers. Return that maximum minimum distance.",
        "Example: [1,2,3,4,7], target=3 -> 3 (place at 1,4,7)\nExample: [1,2,4,8,9], target=3 -> 3 (place at 1,4,8 or 1,4,9)\nExample: [5,4,3,2,1] -> sort first: [1,2,3,4,5], target=2 -> 4 (place at 1,5)",
        "2 <= N <= 100,000\n2 <= target <= N\nPositions may not be sorted.",
        100000, 1000, 300,
        "int digDeep(vector<int>& shaft, int target)",
        "Binary search on minimum distance d. For each candidate d, greedily place markers -- place at first position, then at next position >= last + d. If you can place >= target markers, d is feasible. O(n log range).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a binary-search-on-answer approach",
        "Need to search a monotonic answer space and validate each guess with a feasibility check",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Binary search on minimum distance with greedy placement",
            "Heap of candidate gaps",
            "Dynamic programming over all placements",
            "Union-Find over nearby positions"
        },
        0,
        "Need to search a monotonic answer space and validate each guess with a feasibility check",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given an array `shaft` and integer `target`, split the array into `target` contiguous groups and return the minimum possible value of the largest group sum.";
    waves[0].flavor_text = "Deep in the mine, the night foreman divides ore hauls among exhausted crews working by lamp smoke. The load must be split into contiguous runs, and he wants the smallest maximum burden any one team must carry before dawn.";
    waves[1].clear_prompt = "Given an array `shaft` and integer `target` hours, return the minimum processing speed needed to finish all work within `target` hours.";
    waves[1].flavor_text = "The shafts are filling with dust and bad air. The foreman measures how fast the drills must chew through each ore wall if dawn is to find the tunnels clear and the workers alive.";
    waves[2].clear_prompt = "Given package weights `shaft` and integer `target` days, return the minimum ship capacity that can deliver all packages in order within `target` days.";
    waves[2].flavor_text = "The mine's narrow lift can only carry so much each day. The foreman must choose the smallest capacity that still clears the shipment queue before the old supports begin to fail.";
    waves[3].clear_prompt = "Given sorted or sortable positions in `shaft` and integer `target`, place `target` markers to maximize the minimum distance between any two markers.";
    waves[3].flavor_text = "The tunnels ring with unstable echoes, and warning stakes must be planted before the next tremor. The foreman wants them as far apart as possible while still placing every stake the shaft requires.";
    return waves;
}

void deep_mine::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <algorithm>\n";
        out << "#include <numeric>\n";
        out << "using namespace std;\n\n";

        out << "int digDeep(vector<int>& shaft, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 2501: // Split the Load
                out << "    {\n";
                out << "        vector<int> s = {7, 2, 5, 10, 8};\n";
                out << "        if (digDeep(s, 2) != 18) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 4, 5};\n";
                out << "        if (digDeep(s, 3) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1};\n";
                out << "        if (digDeep(s, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 1, 1, 1, 1};\n";
                out << "        if (digDeep(s, 5) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 4, 5};\n";
                out << "        if (digDeep(s, 1) != 15) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> s(n);\n";
                out << "        for (int i = 0; i < n; i++) s[i] = i % 100 + 1;\n";
                out << "        int target = 10;\n";
                out << "        int result = digDeep(s, target);\n";
                out << "        // Verify: greedily split with max_sum = result, should need <= target groups\n";
                out << "        int groups = 1, cur = 0;\n";
                out << "        for (int i = 0; i < n; i++) {\n";
                out << "            if (cur + s[i] > result) { groups++; cur = s[i]; }\n";
                out << "            else { cur += s[i]; }\n";
                out << "        }\n";
                out << "        if (groups > target) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 2502: // Mining Speed
                out << "    {\n";
                out << "        vector<int> s = {3, 6, 7, 11};\n";
                out << "        if (digDeep(s, 8) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {30, 11, 23, 4, 20};\n";
                out << "        if (digDeep(s, 5) != 30) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {30, 11, 23, 4, 20};\n";
                out << "        if (digDeep(s, 6) != 23) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1};\n";
                out << "        if (digDeep(s, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 1, 1, 1};\n";
                out << "        if (digDeep(s, 4) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {10};\n";
                out << "        if (digDeep(s, 10) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> s(n);\n";
                out << "        for (int i = 0; i < n; i++) s[i] = i % 1000 + 1;\n";
                out << "        int target = n; // plenty of hours\n";
                out << "        int result = digDeep(s, target);\n";
                out << "        // Verify: at speed result, total hours <= target\n";
                out << "        long long hours = 0;\n";
                out << "        for (int i = 0; i < n; i++) hours += (s[i] + result - 1) / result;\n";
                out << "        if (hours > target) all_correct = false;\n";
                out << "        // Verify: at speed result-1, total hours > target (unless result==1)\n";
                out << "        if (result > 1) {\n";
                out << "            hours = 0;\n";
                out << "            for (int i = 0; i < n; i++) hours += (s[i] + result - 2) / (result - 1);\n";
                out << "            if (hours <= target) all_correct = false;\n";
                out << "        }\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 18; }\n";
                break;

            case 2503: // Shipping Capacity
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};\n";
                out << "        if (digDeep(s, 5) != 15) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {3, 2, 2, 4, 1, 4};\n";
                out << "        if (digDeep(s, 3) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 1, 1};\n";
                out << "        if (digDeep(s, 4) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {5};\n";
                out << "        if (digDeep(s, 1) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 1, 1, 1, 1};\n";
                out << "        if (digDeep(s, 1) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 1, 1, 1, 1};\n";
                out << "        if (digDeep(s, 5) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> s(n);\n";
                out << "        for (int i = 0; i < n; i++) s[i] = i % 50 + 1;\n";
                out << "        int target = 100;\n";
                out << "        int result = digDeep(s, target);\n";
                out << "        // Verify: with capacity result, can ship in <= target days\n";
                out << "        int days = 1, cur = 0;\n";
                out << "        for (int i = 0; i < n; i++) {\n";
                out << "            if (cur + s[i] > result) { days++; cur = s[i]; }\n";
                out << "            else { cur += s[i]; }\n";
                out << "        }\n";
                out << "        if (days > target) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 18; }\n";
                break;

            case 2504: // The Placement (boss)
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 4, 7};\n";
                out << "        if (digDeep(s, 3) != 3) all_correct = false; // place at 1,4,7\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 4, 8, 9};\n";
                out << "        if (digDeep(s, 3) != 3) all_correct = false; // place at 1,4,8 or 1,4,9\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {5, 4, 3, 2, 1};\n";
                out << "        if (digDeep(s, 2) != 4) all_correct = false; // sort -> [1,2,3,4,5], place at 1,5\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2};\n";
                out << "        if (digDeep(s, 2) != 1) all_correct = false; // place at 1,2\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 1000000000};\n";
                out << "        if (digDeep(s, 2) != 999999999) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> s = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};\n";
                out << "        if (digDeep(s, 4) != 3) all_correct = false; // place at 1,4,7,10\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> s(n);\n";
                out << "        for (int i = 0; i < n; i++) s[i] = i + 1; // positions 1..n\n";
                out << "        int target = 100;\n";
                out << "        int result = digDeep(s, target);\n";
                out << "        // Expected: floor((n-1)/(target-1))\n";
                out << "        int expected = (n - 1) / (target - 1);\n";
                out << "        if (result != expected) all_correct = false;\n";
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

        out << "const { digDeep } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 2501: // Split the Load
                out << "{\n";
                out << "    let s = [7, 2, 5, 10, 8];\n";
                out << "    if (digDeep(s, 2) !== 18) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2, 3, 4, 5];\n";
                out << "    if (digDeep(s, 3) !== 6) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1];\n";
                out << "    if (digDeep(s, 1) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 1, 1, 1, 1];\n";
                out << "    if (digDeep(s, 5) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2, 3, 4, 5];\n";
                out << "    if (digDeep(s, 1) !== 15) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let s = [];\n";
                out << "    for (let i = 0; i < n; i++) s.push(i % 100 + 1);\n";
                out << "    let target = 10;\n";
                out << "    let result = digDeep(s, target);\n";
                out << "    // Verify: greedily split with max_sum = result, should need <= target groups\n";
                out << "    let groups = 1, cur = 0;\n";
                out << "    for (let i = 0; i < n; i++) {\n";
                out << "        if (cur + s[i] > result) { groups++; cur = s[i]; }\n";
                out << "        else { cur += s[i]; }\n";
                out << "    }\n";
                out << "    if (groups > target) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 2502: // Mining Speed
                out << "{\n";
                out << "    let s = [3, 6, 7, 11];\n";
                out << "    if (digDeep(s, 8) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [30, 11, 23, 4, 20];\n";
                out << "    if (digDeep(s, 5) !== 30) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [30, 11, 23, 4, 20];\n";
                out << "    if (digDeep(s, 6) !== 23) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1];\n";
                out << "    if (digDeep(s, 1) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 1, 1, 1];\n";
                out << "    if (digDeep(s, 4) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [10];\n";
                out << "    if (digDeep(s, 10) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let s = [];\n";
                out << "    for (let i = 0; i < n; i++) s.push(i % 1000 + 1);\n";
                out << "    let target = n; // plenty of hours\n";
                out << "    let result = digDeep(s, target);\n";
                out << "    // Verify: at speed result, total hours <= target\n";
                out << "    let hours = 0;\n";
                out << "    for (let i = 0; i < n; i++) hours += Math.ceil(s[i] / result);\n";
                out << "    if (hours > target) allCorrect = false;\n";
                out << "    // Verify: at speed result-1, total hours > target (unless result==1)\n";
                out << "    if (result > 1) {\n";
                out << "        hours = 0;\n";
                out << "        for (let i = 0; i < n; i++) hours += Math.ceil(s[i] / (result - 1));\n";
                out << "        if (hours <= target) allCorrect = false;\n";
                out << "    }\n";
                out << "    ops = n;\n";
                out << "} else { ops = 18; }\n";
                break;

            case 2503: // Shipping Capacity
                out << "{\n";
                out << "    let s = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];\n";
                out << "    if (digDeep(s, 5) !== 15) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [3, 2, 2, 4, 1, 4];\n";
                out << "    if (digDeep(s, 3) !== 6) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2, 3, 1, 1];\n";
                out << "    if (digDeep(s, 4) !== 3) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [5];\n";
                out << "    if (digDeep(s, 1) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 1, 1, 1, 1];\n";
                out << "    if (digDeep(s, 1) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 1, 1, 1, 1];\n";
                out << "    if (digDeep(s, 5) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let s = [];\n";
                out << "    for (let i = 0; i < n; i++) s.push(i % 50 + 1);\n";
                out << "    let target = 100;\n";
                out << "    let result = digDeep(s, target);\n";
                out << "    // Verify: with capacity result, can ship in <= target days\n";
                out << "    let days = 1, cur = 0;\n";
                out << "    for (let i = 0; i < n; i++) {\n";
                out << "        if (cur + s[i] > result) { days++; cur = s[i]; }\n";
                out << "        else { cur += s[i]; }\n";
                out << "    }\n";
                out << "    if (days > target) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 18; }\n";
                break;

            case 2504: // The Placement (boss)
                out << "{\n";
                out << "    let s = [1, 2, 3, 4, 7];\n";
                out << "    if (digDeep(s, 3) !== 3) allCorrect = false; // place at 1,4,7\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2, 4, 8, 9];\n";
                out << "    if (digDeep(s, 3) !== 3) allCorrect = false; // place at 1,4,8 or 1,4,9\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [5, 4, 3, 2, 1];\n";
                out << "    if (digDeep(s, 2) !== 4) allCorrect = false; // sort -> [1,2,3,4,5], place at 1,5\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2];\n";
                out << "    if (digDeep(s, 2) !== 1) allCorrect = false; // place at 1,2\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 1000000000];\n";
                out << "    if (digDeep(s, 2) !== 999999999) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let s = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];\n";
                out << "    if (digDeep(s, 4) !== 3) allCorrect = false; // place at 1,4,7,10\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let s = [];\n";
                out << "    for (let i = 0; i < n; i++) s.push(i + 1); // positions 1..n\n";
                out << "    let target = 100;\n";
                out << "    let result = digDeep(s, target);\n";
                out << "    // Expected: floor((n-1)/(target-1))\n";
                out << "    let expected = Math.floor((n - 1) / (target - 1));\n";
                out << "    if (result !== expected) allCorrect = false;\n";
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
