#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> bazaar::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1601, "Maximum Profit", WORLD_NAME, WORLD_DESC,
        "Best time to buy and sell stock once. deals[i] is the price on day i. Find the maximum profit from one buy then one sell. Return 0 if no profit is possible. target is unused.",
        "Example: [7,1,5,3,6,4] -> 5 (buy at 1, sell at 6)\nExample: [7,6,4,3,1] -> 0\nExample: [1,2] -> 1\nExample: [2,4,1] -> 2",
        "1 <= N <= 500,000",
        500000, 1000, 300,
        "int tradeBazaar(vector<int>& deals, int target)",
        "Track minimum price seen so far. At each price, profit = price - min_so_far. Track maximum profit. O(n) single pass.", generateStub, generateRunner
    });

    waves.push_back({
        1602, "Jump the Stalls", WORLD_NAME, WORLD_DESC,
        "deals[i] is the maximum jump length from position i. Return 1 if you can reach the last index, 0 otherwise. target is unused.",
        "Example: [2,3,1,1,4] -> 1\nExample: [3,2,1,0,4] -> 0\nExample: [0] -> 1\nExample: [1,1,1,1] -> 1",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int tradeBazaar(vector<int>& deals, int target)",
        "Track the farthest reachable index. For each position, update farthest = max(farthest, i + nums[i]). If i > farthest, can't proceed. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1603, "Minimum Jumps", WORLD_NAME, WORLD_DESC,
        "Return the minimum number of jumps to reach the last index. deals[i] is the maximum jump length from position i. Input is always reachable. target is unused.",
        "Example: [2,3,1,1,4] -> 2\nExample: [1,1,1,1] -> 3\nExample: [5,1,1,1,1] -> 1\nExample: [1] -> 0",
        "1 <= N <= 100,000\nAlways reachable.",
        100000, 1000, 300,
        "int tradeBazaar(vector<int>& deals, int target)",
        "Greedy BFS approach. Track current range end and farthest reachable. When you pass current end, increment jumps and extend to farthest. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1604, "Gas Station", WORLD_NAME, WORLD_DESC,
        "deals has 2n elements. The first n are gas[i] at each station, the last n are cost[i] to travel to the next station. Return the starting station index for a circular tour that completes the circuit, or -1 if impossible. target is unused.",
        "Example: [1,2,3,4,5,3,4,5,1,2] -> 3 (gas=[1,2,3,4,5], cost=[3,4,5,1,2])\nExample: [2,3,4,3,4,5] -> -1 (gas=[2,3,4], cost=[3,4,5])",
        "2 <= deals.size() <= 200,000 (even)\ntarget is unused.",
        100000, 1000, 300,
        "int tradeBazaar(vector<int>& deals, int target)",
        "Track total surplus and current surplus. If current goes negative, reset starting point to next station. If total surplus >= 0, the last reset point is the answer. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1605, "The Auction", WORLD_NAME, WORLD_DESC,
        "Boss wave. deals contains children's greed factors followed by -1 followed by cookie sizes. Assign cookies to children: a child is content if cookie >= greed. Each cookie can go to at most one child. Return max number of content children. target is unused.",
        "Example: [1,2,-1,1,2,3] -> 2 (greed=[1,2], cookies=[1,2,3])\nExample: [1,2,3,-1,1,1] -> 1\nExample: [10,-1,1,2,3] -> 0",
        "1 <= total elements <= 100,000\ntarget is unused.",
        50000, 1000, 300,
        "int tradeBazaar(vector<int>& deals, int target)",
        "Sort both greed factors and cookie sizes. Two pointers -- try to satisfy smallest greed with smallest sufficient cookie. O(n log n) for sorting.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a greedy approach",
        "Need locally optimal choices that preserve the global best answer",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Sort both sides and greedily match with two pointers",
            "Dynamic programming over all assignments",
            "Heap of every cookie-child pairing",
            "Binary search each child independently"
        },
        0,
        "Need locally optimal choices that preserve the global best answer",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given an array `deals` where `deals[i]` is the price on day `i`, return the maximum profit from buying once and selling once later. Return `0` if no profit is possible.";
    waves[0].flavor_text = "Nima the broker walks the lantern bazaar with a slate tucked under her arm, watching prices rise and fall like heat. She wants one clean buy and one clean sell, timed so well that profit arrives before rumor does.";
    waves[1].clear_prompt = "Given an array `deals` where `deals[i]` is the maximum jump length from index `i`, return `1` if the last index is reachable and `0` otherwise.";
    waves[1].flavor_text = "The market's raised planks are collapsing into gaps one by one. Nima has to decide whether the route can still be crossed at all before the whole bazaar drops into the drainage canals below.";
    waves[2].clear_prompt = "Given an array `deals` where `deals[i]` is the maximum jump length from index `i`, return the minimum number of jumps needed to reach the last index.";
    waves[2].flavor_text = "This time Nima already knows the line can be crossed. What matters is speed: the fewest leaps, the least time exposed above the gaps, and the cleanest route through a market floor that keeps failing under her feet.";
    waves[3].clear_prompt = "Given an array `deals` encoding `gas` values followed by `cost` values as described by the wave, return the start index that completes the circuit or `-1` if none exists.";
    waves[3].flavor_text = "A fuel caravan circles the bazaar walls at dawn, and Nima gets exactly one chance to choose the right gate to start from. Pick badly and the beasts stall before the circuit closes; pick well and the whole ring comes home intact.";
    waves[4].clear_prompt = "Given greed factors and cookie sizes encoded in `deals` as described by the wave, return the maximum number of children that can be satisfied.";
    waves[4].flavor_text = "At auction's end, the sweet-cakes are too few and the waiting hands too many. Nima has to match each treat to the smallest hunger it can satisfy and rescue as many bargains as possible before the candles burn out.";
    return waves;
}

void bazaar::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int tradeBazaar(vector<int>& deals, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1601: // Maximum Profit
            out << "    {\n";
            out << "        vector<int> d = {7, 1, 5, 3, 6, 4};\n";
            out << "        if (tradeBazaar(d, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {7, 6, 4, 3, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 2};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {2, 4, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {3, 3, 3};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> d(n);\n";
            out << "        for (int i = 0; i < n; i++) d[i] = i; // ascending: 0,1,2,...,n-1\n";
            out << "        if (tradeBazaar(d, 0) != n - 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 1602: // Jump the Stalls
            out << "    {\n";
            out << "        vector<int> d = {2, 3, 1, 1, 4};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {3, 2, 1, 0, 4};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {0};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 1, 1, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {0, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {2, 0, 0};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> d(n, 1); // all 1s: reachable\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 1603: // Minimum Jumps
            out << "    {\n";
            out << "        vector<int> d = {2, 3, 1, 1, 4};\n";
            out << "        if (tradeBazaar(d, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 1, 1, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {5, 1, 1, 1, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {2, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 2, 3};\n";
            out << "        if (tradeBazaar(d, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> d(n, n); // all values = n: reachable in 1 jump\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 1604: // Gas Station
            out << "    {\n";
            out << "        vector<int> d = {1, 2, 3, 4, 5, 3, 4, 5, 1, 2};\n";
            out << "        if (tradeBazaar(d, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {2, 3, 4, 3, 4, 5};\n";
            out << "        if (tradeBazaar(d, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {5, 1, 2, 3, 4, 4, 4, 5, 1, 2};\n";
            out << "        if (tradeBazaar(d, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {3, 1, 1, 0};\n";
            out << "        // gas=[3,1], cost=[1,0]; start at 0: 3-1=2, 2+1-0=3 -> 0\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 1, 2, 2};\n";
            out << "        // gas=[1,1], cost=[2,2]; total gas=2 < total cost=4 -> -1\n";
            out << "        if (tradeBazaar(d, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> d(2 * n);\n";
            out << "        // gas[i] = 1 for all, cost[i] = 1 for all -> any start works, return 0\n";
            out << "        for (int i = 0; i < n; i++) { d[i] = 1; d[n + i] = 1; }\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 1605: // The Auction (boss)
            out << "    {\n";
            out << "        vector<int> d = {1, 2, -1, 1, 2, 3};\n";
            out << "        if (tradeBazaar(d, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 2, 3, -1, 1, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {10, -1, 1, 2, 3};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {1, 1, 1, -1, 1, 1, 1};\n";
            out << "        if (tradeBazaar(d, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {5, -1, 3, 4, 5};\n";
            out << "        if (tradeBazaar(d, 0) != 1) all_correct = false; // cookie 5 satisfies greed 5\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> d = {-1, 1, 2, 3};\n";
            out << "        if (tradeBazaar(d, 0) != 0) all_correct = false; // no children\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        // n children with greed 1..n, n cookies with size 1..n\n";
            out << "        vector<int> d;\n";
            out << "        d.reserve(2 * n + 1);\n";
            out << "        for (int i = 1; i <= n; i++) d.push_back(i);\n";
            out << "        d.push_back(-1);\n";
            out << "        for (int i = 1; i <= n; i++) d.push_back(i);\n";
            out << "        if (tradeBazaar(d, 0) != n) all_correct = false;\n";
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

    out << "const { tradeBazaar } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1601: // Maximum Profit
            out << "{\n";
            out << "    let d = [7, 1, 5, 3, 6, 4];\n";
            out << "    if (tradeBazaar(d, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [7, 6, 4, 3, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 2];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [2, 4, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [3, 3, 3];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let d = [];\n";
            out << "    for (let i = 0; i < n; i++) d.push(i);\n";
            out << "    if (tradeBazaar(d, 0) !== n - 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 1602: // Jump the Stalls
            out << "{\n";
            out << "    let d = [2, 3, 1, 1, 4];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [3, 2, 1, 0, 4];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [0];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 1, 1, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [0, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [2, 0, 0];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let d = new Array(n).fill(1);\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 1603: // Minimum Jumps
            out << "{\n";
            out << "    let d = [2, 3, 1, 1, 4];\n";
            out << "    if (tradeBazaar(d, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 1, 1, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [5, 1, 1, 1, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [2, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 2, 3];\n";
            out << "    if (tradeBazaar(d, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let d = new Array(n).fill(n);\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 1604: // Gas Station
            out << "{\n";
            out << "    let d = [1, 2, 3, 4, 5, 3, 4, 5, 1, 2];\n";
            out << "    if (tradeBazaar(d, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [2, 3, 4, 3, 4, 5];\n";
            out << "    if (tradeBazaar(d, 0) !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [5, 1, 2, 3, 4, 4, 4, 5, 1, 2];\n";
            out << "    if (tradeBazaar(d, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [3, 1, 1, 0];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 1, 2, 2];\n";
            out << "    if (tradeBazaar(d, 0) !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let d = new Array(2 * n);\n";
            out << "    for (let i = 0; i < n; i++) { d[i] = 1; d[n + i] = 1; }\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 1605: // The Auction (boss)
            out << "{\n";
            out << "    let d = [1, 2, -1, 1, 2, 3];\n";
            out << "    if (tradeBazaar(d, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 2, 3, -1, 1, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [10, -1, 1, 2, 3];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [1, 1, 1, -1, 1, 1, 1];\n";
            out << "    if (tradeBazaar(d, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [5, -1, 3, 4, 5];\n";
            out << "    if (tradeBazaar(d, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let d = [-1, 1, 2, 3];\n";
            out << "    if (tradeBazaar(d, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let d = [];\n";
            out << "    for (let i = 1; i <= n; i++) d.push(i);\n";
            out << "    d.push(-1);\n";
            out << "    for (let i = 1; i <= n; i++) d.push(i);\n";
            out << "    if (tradeBazaar(d, 0) !== n) allCorrect = false;\n";
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
