#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> porthole::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        801, "Fixed Lens", WORLD_NAME, WORLD_DESC,
        "Find the maximum sum of any contiguous subarray of exactly target length.",
        "Example: [1,3,-1,2,5,1], target=3 -> 8 (subarray [2,5,1])\nExample: [4,2,1,7,8,1,2,8,1,0], target=3 -> 16 (7+8+1)\nExample: [1], target=1 -> 1",
        "1 <= target <= N <= 500,000",
        500000, 1000, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Sum the first k elements. Slide the window: subtract the element leaving, add the element entering. Track the maximum sum seen. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        802, "Average Scan", WORLD_NAME, WORLD_DESC,
        "Find the maximum average of any contiguous subarray of at least target length. Return floor(max_average * 100) for integer comparison.",
        "Example: [1,12,-5,-6,50,3], target=4 -> 1283 (subarray [12,-5,-6,50,3] avg=10.8, but [1,12,-5,-6,50,3] avg=9.16... best is [12,-5,-6,50] avg=12.75 -> 1275? Let me re-check: [50,3]=26.5 but len<4. [-6,50,3]=15.67 len<4. [-5,-6,50,3]=10.5 len=4. [12,-5,-6,50]=12.75 len=4. [12,-5,-6,50,3]=10.8 len=5. Best=1275)\nExample: [5], target=1 -> 500",
        "1 <= target <= N <= 100,000",
        100000, 2000, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Use prefix sums or binary search on the answer. For each starting position, expand to the minimum length and track the best average. O(n) with the prefix sum approach.", generateStub, generateRunner
    });

    waves.push_back({
        803, "Smallest Catch", WORLD_NAME, WORLD_DESC,
        "Find the length of the smallest contiguous subarray whose sum is >= target. Return 0 if no such subarray exists.",
        "Example: [2,3,1,2,4,3], target=7 -> 2 (subarray [4,3])\nExample: [1,1,1,1,1], target=11 -> 0\nExample: [1,4,4], target=4 -> 1",
        "1 <= N <= 500,000\n1 <= target",
        500000, 1000, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Variable-size window. Expand the right end; when the sum >= target, try shrinking from the left. Track the minimum window length that satisfies the condition. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        804, "Longest Run", WORLD_NAME, WORLD_DESC,
        "Find the length of the longest contiguous subarray where all elements are the same value. target is unused.",
        "Example: [1,1,2,2,2,3,3] -> 3 (three 2s)\nExample: [1,2,3,4,5] -> 1\nExample: [7,7,7,7] -> 4\nExample: [5] -> 1",
        "1 <= N <= 500,000\ntarget is unused.",
        500000, 1000, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Single pass. Track the current value and run length. Reset the run length when the value changes. Track the maximum run length seen. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        805, "Longest Calm Waters", WORLD_NAME, WORLD_DESC,
        "Find the length of the longest contiguous subarray where max - min <= target.",
        "Example: [1,3,2,4,6], target=2 -> 3 (subarray [1,3,2] or [3,2,4])\nExample: [1,1,1,1], target=0 -> 4\nExample: [10,1,10,1], target=0 -> 1",
        "1 <= N <= 500,000\n0 <= target",
        500000, 1000, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Variable window with two monotonic deques tracking the window max and min. Expand the right end, shrink the left when max - min exceeds the target. Track the longest valid window. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        806, "The Deep Signal", WORLD_NAME, WORLD_DESC,
        "Count the number of contiguous subarrays where max - min <= target.",
        "Example: [1,3,2], target=2 -> 6 ([1],[3],[2],[1,3],[3,2],[1,3,2] all have max-min<=2)\nExample: [1,3,2], target=0 -> 3 (only single elements)\nExample: [1,1,1], target=0 -> 6",
        "1 <= N <= 500,000\n0 <= target",
        500000, 500, 300,
        "int watchTheFlow(vector<int>& current, int target)",
        "Same deque approach as the previous wave. For each right endpoint, find the leftmost valid left endpoint. All subarrays ending at right with left in [leftmost..right] are valid. Count += right - leftmost + 1. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a sliding-window approach",
        "Need to maintain a contiguous window while updating counts incrementally",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Sliding window with incremental range tracking",
            "Dynamic programming over all windows",
            "Union-Find over adjacent indices",
            "Trie of window signatures"
        },
        0,
        "Need to maintain a contiguous window while updating counts incrementally",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given an array and a fixed window size, compute the required aggregate for every contiguous window of that size as specified by the wave.";
    waves[0].flavor_text = "The ship's watch officer peers through a salt-fogged porthole, scanning the sea in fixed slices. She cannot watch the whole horizon at once, only one narrow pane after another, measuring what passes through each frame.";
    waves[1].clear_prompt = "Given an array and a fixed window size, return the maximum or average value requested by the wave across all contiguous windows of that size.";
    waves[1].flavor_text = "The watch officer steadies herself against the ship's hull, counting waves through a round pane of glass. Each shift of the view reveals a new strip of motion, and she wants the strongest reading any fixed glimpse can offer.";
    waves[2].clear_prompt = "Given an array and a target condition, find the smallest contiguous subarray that satisfies that condition.";
    waves[2].flavor_text = "The watch officer tracks a signal in the storm but refuses to waste attention on a window larger than necessary. She tightens the search frame until only the smallest useful span remains.";
    waves[3].clear_prompt = "Given an array and a target condition, find the longest contiguous subarray that remains valid under that condition.";
    waves[3].flavor_text = "The watch officer tests how long a smooth current can last before the sea turns violent again. She slides her gaze along the water, stretching the window until the calm finally breaks.";
    waves[4].clear_prompt = "Given an array, return the longest contiguous subarray that satisfies the calm-water or all-valid condition defined by the wave.";
    waves[4].flavor_text = "Far below the hull, the dark water stills for brief, precious spans. The watch officer listens for the longest stretch where nothing in the pattern breaks the illusion of peace.";
    waves[5].clear_prompt = "Given an array or string and a target condition, return the best contiguous window that satisfies the full signal requirement described by the wave.";
    waves[5].flavor_text = "The porthole blooms with phosphorescent light, and the watch officer knows the full omen is hiding in one continuous band of sea. She must keep the smallest view that still captures the entire sign.";
    return waves;
}

void porthole::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <vector>\n";
    out << "#include <string>\n";
    out << "#include <deque>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <algorithm>\n";
    out << "using namespace std;\n\n";

    out << "int watchTheFlow(vector<int>& current, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 801:
            out << "    {\n";
            out << "        vector<int> c = {1, 3, -1, 2, 5, 1};\n";
            out << "        if (watchTheFlow(c, 3) != 8) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {4, 2, 1, 7, 8, 1, 2, 8, 1, 0};\n";
            out << "        if (watchTheFlow(c, 3) != 16) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1};\n";
            out << "        if (watchTheFlow(c, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-1, -2, -3, -4};\n";
            out << "        if (watchTheFlow(c, 2) != -3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5, 5, 5, 5};\n";
            out << "        if (watchTheFlow(c, 4) != 20) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n, 1);\n";
            out << "        c[n / 2] = n;\n";
            out << "        int k = 100;\n";
            out << "        if (watchTheFlow(c, k) < n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 802:
            out << "    {\n";
            out << "        vector<int> c = {1, 12, -5, -6, 50, 3};\n";
            out << "        if (watchTheFlow(c, 4) != 1275) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5};\n";
            out << "        if (watchTheFlow(c, 1) != 500) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (watchTheFlow(c, 2) != 450) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {10, 10, 10};\n";
            out << "        if (watchTheFlow(c, 1) != 1000) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n, 1);\n";
            out << "        c[0] = n;\n";
            out << "        if (watchTheFlow(c, 1) != n * 100) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 803:
            out << "    {\n";
            out << "        vector<int> c = {2, 3, 1, 2, 4, 3};\n";
            out << "        if (watchTheFlow(c, 7) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 1, 1, 1};\n";
            out << "        if (watchTheFlow(c, 11) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 4, 4};\n";
            out << "        if (watchTheFlow(c, 4) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (watchTheFlow(c, 15) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {10};\n";
            out << "        if (watchTheFlow(c, 5) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n, 1);\n";
            out << "        c[n - 1] = n;\n";
            out << "        if (watchTheFlow(c, n) != 1) all_correct = false;\n";
            out << "        if (watchTheFlow(c, 2 * n) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 15; }\n";
            break;

        case 804:
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 2, 2, 2, 3, 3};\n";
            out << "        if (watchTheFlow(c, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {7, 7, 7, 7};\n";
            out << "        if (watchTheFlow(c, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5};\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 2, 1, 1, 1, 3};\n";
            out << "        if (watchTheFlow(c, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n, 5);\n";
            out << "        if (watchTheFlow(c, 0) != n) all_correct = false;\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i % 2;\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 805:
            out << "    {\n";
            out << "        vector<int> c = {1, 3, 2, 4, 6};\n";
            out << "        if (watchTheFlow(c, 2) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 1, 1};\n";
            out << "        if (watchTheFlow(c, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {10, 1, 10, 1};\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (watchTheFlow(c, 3) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5};\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {4, 2, 2, 4};\n";
            out << "        if (watchTheFlow(c, 2) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i % 10;\n";
            out << "        if (watchTheFlow(c, 9) != n) all_correct = false;\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 806:
            out << "    {\n";
            out << "        vector<int> c = {1, 3, 2};\n";
            out << "        if (watchTheFlow(c, 2) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 3, 2};\n";
            out << "        if (watchTheFlow(c, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 1};\n";
            out << "        if (watchTheFlow(c, 0) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3};\n";
            out << "        if (watchTheFlow(c, 1) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5};\n";
            out << "        if (watchTheFlow(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n, 1);\n";
            out << "        long long expected = (long long)n * (n + 1) / 2;\n";
            out << "        if (watchTheFlow(c, 0) != (int)expected) all_correct = false;\n";
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

    out << "const { watchTheFlow } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 801: // Fixed Lens
            out << "if (watchTheFlow([1, 3, -1, 2, 5, 1], 3) !== 8) allCorrect = false;\n";
            out << "if (watchTheFlow([4, 2, 1, 7, 8, 1, 2, 8, 1, 0], 3) !== 16) allCorrect = false;\n";
            out << "if (watchTheFlow([1], 1) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([-1, -2, -3, -4], 2) !== -3) allCorrect = false;\n";
            out << "if (watchTheFlow([5, 5, 5, 5], 4) !== 20) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n).fill(1);\n";
            out << "    c[Math.floor(n / 2)] = n;\n";
            out << "    let k = 100;\n";
            out << "    if (watchTheFlow(c, k) < n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 802: // Average Scan
            out << "if (watchTheFlow([1, 12, -5, -6, 50, 3], 4) !== 1275) allCorrect = false;\n";
            out << "if (watchTheFlow([5], 1) !== 500) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 3, 4, 5], 2) !== 450) allCorrect = false;\n";
            out << "if (watchTheFlow([10, 10, 10], 1) !== 1000) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n).fill(1);\n";
            out << "    c[0] = n;\n";
            out << "    if (watchTheFlow(c, 1) !== n * 100) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 803: // Smallest Catch
            out << "if (watchTheFlow([2, 3, 1, 2, 4, 3], 7) !== 2) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 1, 1, 1, 1], 11) !== 0) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 4, 4], 4) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 3, 4, 5], 15) !== 5) allCorrect = false;\n";
            out << "if (watchTheFlow([10], 5) !== 1) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n).fill(1);\n";
            out << "    c[n - 1] = n;\n";
            out << "    if (watchTheFlow(c, n) !== 1) allCorrect = false;\n";
            out << "    if (watchTheFlow(c, 2 * n) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 15; }\n";
            break;

        case 804: // Longest Run
            out << "if (watchTheFlow([1, 1, 2, 2, 2, 3, 3], 0) !== 3) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 3, 4, 5], 0) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([7, 7, 7, 7], 0) !== 4) allCorrect = false;\n";
            out << "if (watchTheFlow([5], 0) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 2, 1, 1, 1, 3], 0) !== 3) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n).fill(5);\n";
            out << "    if (watchTheFlow(c, 0) !== n) allCorrect = false;\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i % 2;\n";
            out << "    if (watchTheFlow(c, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 805: // Longest Calm Waters
            out << "if (watchTheFlow([1, 3, 2, 4, 6], 2) !== 3) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 1, 1, 1], 0) !== 4) allCorrect = false;\n";
            out << "if (watchTheFlow([10, 1, 10, 1], 0) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 3, 4, 5], 3) !== 4) allCorrect = false;\n";
            out << "if (watchTheFlow([5], 0) !== 1) allCorrect = false;\n";
            out << "if (watchTheFlow([4, 2, 2, 4], 2) !== 4) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = [];\n";
            out << "    for (let i = 0; i < n; i++) c.push(i % 10);\n";
            out << "    if (watchTheFlow(c, 9) !== n) allCorrect = false;\n";
            out << "    if (watchTheFlow(c, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 806: // The Deep Signal (boss)
            out << "if (watchTheFlow([1, 3, 2], 2) !== 6) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 3, 2], 0) !== 3) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 1, 1], 0) !== 6) allCorrect = false;\n";
            out << "if (watchTheFlow([1, 2, 3], 1) !== 5) allCorrect = false;\n";
            out << "if (watchTheFlow([5], 0) !== 1) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n).fill(1);\n";
            out << "    let expected = n * (n + 1) / 2;\n";
            out << "    if (watchTheFlow(c, 0) !== expected) allCorrect = false;\n";
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
