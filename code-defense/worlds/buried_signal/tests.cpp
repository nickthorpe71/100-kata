#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> buried_signal::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        901, "Lock the Frequency", WORLD_NAME, WORLD_DESC,
        "Classic binary search: find target in the sorted array. Return its index, or -1 if not found.",
        "Example: [1,3,5,7,9], target=5 -> 2\nExample: [1,3,5,7,9], target=4 -> -1\nExample: [1], target=1 -> 0\nExample: [2,4,6], target=6 -> 2",
        "1 <= N <= 1,000,000\nArray is sorted ascending.\nAll elements are unique.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Binary search: compare the middle element to the target. If equal, found. If less, search the right half. If greater, search the left half. O(log n).", generateStub, generateRunner
    });

    waves.push_back({
        902, "Lower Bound", WORLD_NAME, WORLD_DESC,
        "Find the first position where value >= target. Return that index. Return N if all elements are less than target.",
        "Example: [1,3,3,5,7], target=3 -> 1\nExample: [1,3,5,7], target=4 -> 2\nExample: [1,3,5,7], target=8 -> 4\nExample: [1,3,5,7], target=0 -> 0",
        "1 <= N <= 1,000,000\nArray is sorted ascending.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Binary search variant. If mid >= target, record mid as a candidate and search left. Otherwise search right. Return the leftmost candidate. O(log n).", generateStub, generateRunner
    });

    waves.push_back({
        903, "Count in Range", WORLD_NAME, WORLD_DESC,
        "Count elements in the range [target, target + bands[0]]. bands[0] encodes the range width, and the searchable array starts at bands[1]. Array is sorted.",
        "Example: [3, 1,2,3,4,5,6,7], target=2 -> 4 (range [2,5], elements 2,3,4,5)\nExample: [0, 1,3,5,7], target=3 -> 1 (range [3,3], just 3)\nExample: [10, 1,2,3], target=5 -> 0 (range [5,15], nothing)",
        "1 <= N <= 1,000,000\nArray (from index 1) is sorted ascending.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Apply lower_bound for the range start and upper_bound for range end + 1. The difference gives the count. Two binary searches. O(log n).", generateStub, generateRunner
    });

    waves.push_back({
        904, "Peak Frequency", WORLD_NAME, WORLD_DESC,
        "Array is bitonic: strictly increases then strictly decreases. Find and return the peak value. target is unused.",
        "Example: [1,3,5,4,2] -> 5\nExample: [1,5,3] -> 5\nExample: [10,8,6,4,2] -> 10 (peak at start)\nExample: [1,2,3,4,5] -> 5 (peak at end)",
        "1 <= N <= 1,000,000\ntarget is unused.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Binary search. If mid < mid+1, the peak is to the right. If mid > mid+1, the peak is to the left or at mid. Converge on the peak element. O(log n).", generateStub, generateRunner
    });

    waves.push_back({
        905, "Search the Rotation", WORLD_NAME, WORLD_DESC,
        "Array was sorted ascending then rotated. Find target, return its index or -1. All elements are unique.",
        "Example: [4,5,6,7,0,1,2], target=0 -> 4\nExample: [4,5,6,7,0,1,2], target=3 -> -1\nExample: [1], target=1 -> 0\nExample: [3,1,2], target=2 -> 2",
        "1 <= N <= 1,000,000\nAll elements unique.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Binary search on a rotated sorted array. Check which half is sorted by comparing mid to endpoints. If the target is in the sorted half, search there; otherwise search the other half. O(log n).", generateStub, generateRunner
    });

    waves.push_back({
        906, "The Resonance", WORLD_NAME, WORLD_DESC,
        "Array was sorted ascending then rotated. Find and return the minimum element. target is unused.",
        "Example: [4,5,6,7,0,1,2] -> 0\nExample: [3,4,5,1,2] -> 1\nExample: [1,2,3,4,5] -> 1 (no rotation)\nExample: [2,1] -> 1",
        "1 <= N <= 1,000,000\nAll elements unique.\ntarget is unused.",
        1000000, 500, 300,
        "int dialIn(vector<int>& bands, int target)",
        "Binary search. If mid > right, the minimum is in the right half. If mid < right, the minimum is in the left half (including mid). Converge on the minimum element. O(log n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a binary-search-based approach",
        "Need ordered or monotonic structure so each comparison discards half the search space",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Binary search the answer with a greedy partition validator",
            "Prefix sum hash map",
            "Stack-based monotonic scan",
            "Trie over numeric digits"
        },
        0,
        "Need ordered or monotonic structure so each comparison discards half the search space",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given a sorted array and a target value, return the exact-match index or the failure value specified by the wave.";
    waves[0].flavor_text = "A desert signal hunter kneels beside a line of calibrated rods, listening for one exact resonance buried in ordered layers of sand and stone. Each probe halves the silence around the answer.";
    waves[1].clear_prompt = "Given a sorted array and a target value, return the lower-bound index or insertion position described by the wave.";
    waves[1].flavor_text = "Finding the signal is not enough for the hunter. She wants the earliest place it could honestly begin, the leftmost fracture in the buried layers where that frequency still belongs.";
    waves[2].clear_prompt = "Given a sorted array and a target range condition, return the number of values or bounds requested by the wave.";
    waves[2].flavor_text = "Several readings are packed into the same seam beneath the dunes. The hunter must find where the target signal enters the range, where it leaves, and how much of the underground choir truly belongs to it.";
    waves[3].clear_prompt = "Given a bitonic or structured array, return the peak or turning-point value/index requested by the wave.";
    waves[3].flavor_text = "The resonance rises, crests, and falls again like a tremor through buried masonry. The hunter wants the exact turning point where the hidden frequency stands at full strength.";
    waves[4].clear_prompt = "Given a rotated sorted array and a target value, return the target's index or the wave-specific failure value.";
    waves[4].flavor_text = "Time has twisted the survey map, but the ordering still survives beneath the rotation. The hunter traces the break in the pattern and uses it to track the signal anyway.";
    waves[5].clear_prompt = "Given the array and target rule described by the boss wave, return the minimum feasible answer found by binary searching the solution space.";
    waves[5].flavor_text = "The resonance chamber under the dunes will only open for the smallest capacity, force, or threshold that still works. Anything larger wastes precious charge; anything smaller leaves the hunter sealed in the dark.";
    return waves;
}

void buried_signal::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

        out << "int dialIn(vector<int>& bands, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 901: // Lock the Frequency
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 7, 9};\n";
                out << "        if (dialIn(b, 5) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 7, 9};\n";
                out << "        if (dialIn(b, 4) != -1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1};\n";
                out << "        if (dialIn(b, 1) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {2, 4, 6};\n";
                out << "        if (dialIn(b, 6) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {2, 4, 6};\n";
                out << "        if (dialIn(b, 2) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 2, 3, 4, 5};\n";
                out << "        if (dialIn(b, 0) != -1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n);\n";
                out << "        for (int i = 0; i < n; i++) b[i] = i * 2;\n";
                out << "        // Search for last element\n";
                out << "        if (dialIn(b, (n - 1) * 2) != n - 1) all_correct = false;\n";
                out << "        // Search for missing element\n";
                out << "        if (dialIn(b, 1) != -1) all_correct = false;\n";
                out << "        // Search for first element\n";
                out << "        if (dialIn(b, 0) != 0) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 17; }\n";
                break;

            case 902: // Lower Bound
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 3, 5, 7};\n";
                out << "        if (dialIn(b, 3) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 7};\n";
                out << "        if (dialIn(b, 4) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 7};\n";
                out << "        if (dialIn(b, 8) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 7};\n";
                out << "        if (dialIn(b, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {5, 5, 5, 5};\n";
                out << "        if (dialIn(b, 5) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1};\n";
                out << "        if (dialIn(b, 1) != 0) all_correct = false;\n";
                out << "        if (dialIn(b, 2) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n);\n";
                out << "        for (int i = 0; i < n; i++) b[i] = i * 2; // 0,2,4,...\n";
                out << "        // Lower bound of 1 -> index 1 (first val >= 1 is 2 at index 1)\n";
                out << "        if (dialIn(b, 1) != 1) all_correct = false;\n";
                out << "        // Lower bound of 0 -> index 0\n";
                out << "        if (dialIn(b, 0) != 0) all_correct = false;\n";
                out << "        // Lower bound beyond max -> n\n";
                out << "        if (dialIn(b, n * 2) != n) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 18; }\n";
                break;

            case 903: // Count in Range
                out << "    {\n";
                out << "        vector<int> b = {3, 1, 2, 3, 4, 5, 6, 7};\n";
                out << "        if (dialIn(b, 2) != 4) all_correct = false; // [2,5] -> 2,3,4,5\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {0, 1, 3, 5, 7};\n";
                out << "        if (dialIn(b, 3) != 1) all_correct = false; // [3,3] -> just 3\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {10, 1, 2, 3};\n";
                out << "        if (dialIn(b, 5) != 0) all_correct = false; // [5,15] -> nothing\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {100, 1, 2, 3, 4, 5};\n";
                out << "        if (dialIn(b, 1) != 5) all_correct = false; // [1,101] -> all 5\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {0, 5, 5, 5, 5};\n";
                out << "        if (dialIn(b, 5) != 4) all_correct = false; // [5,5] -> all four 5s\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n + 1);\n";
                out << "        b[0] = 100; // range width\n";
                out << "        for (int i = 1; i <= n; i++) b[i] = i;\n";
                out << "        // [500, 600] — count elements in that range\n";
                out << "        int lo = 500, hi = 600;\n";
                out << "        int expected = min(hi, n) - lo + 1;\n";
                out << "        if (expected < 0) expected = 0;\n";
                out << "        if (dialIn(b, lo) != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 904: // Peak Frequency
                out << "    {\n";
                out << "        vector<int> b = {1, 3, 5, 4, 2};\n";
                out << "        if (dialIn(b, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 5, 3};\n";
                out << "        if (dialIn(b, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {10, 8, 6, 4, 2};\n";
                out << "        if (dialIn(b, 0) != 10) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 2, 3, 4, 5};\n";
                out << "        if (dialIn(b, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 10, 2};\n";
                out << "        if (dialIn(b, 0) != 10) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {42};\n";
                out << "        if (dialIn(b, 0) != 42) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n);\n";
                out << "        int peak = n / 3;\n";
                out << "        for (int i = 0; i <= peak; i++) b[i] = i;\n";
                out << "        for (int i = peak + 1; i < n; i++) b[i] = peak - (i - peak);\n";
                out << "        if (dialIn(b, 0) != peak) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 17; }\n";
                break;

            case 905: // Search the Rotation
                out << "    {\n";
                out << "        vector<int> b = {4, 5, 6, 7, 0, 1, 2};\n";
                out << "        if (dialIn(b, 0) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {4, 5, 6, 7, 0, 1, 2};\n";
                out << "        if (dialIn(b, 3) != -1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1};\n";
                out << "        if (dialIn(b, 1) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {3, 1, 2};\n";
                out << "        if (dialIn(b, 2) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 2, 3, 4, 5};\n";
                out << "        if (dialIn(b, 5) != 4) all_correct = false; // no rotation\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {5, 1, 2, 3, 4};\n";
                out << "        if (dialIn(b, 5) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {2, 3, 4, 5, 1};\n";
                out << "        if (dialIn(b, 1) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n);\n";
                out << "        int rot = n / 3;\n";
                out << "        // Rotated: [rot, rot+1, ..., n-1, 0, 1, ..., rot-1]\n";
                out << "        for (int i = 0; i < n; i++) b[i] = (i + rot) % n;\n";
                out << "        // Search for 0 — should be at position n - rot\n";
                out << "        if (dialIn(b, 0) != n - rot) all_correct = false;\n";
                out << "        // Search for n-1 — should be at position n - rot - 1\n";
                out << "        if (dialIn(b, n - 1) != n - rot - 1) all_correct = false;\n";
                out << "        // Search for n — not present\n";
                out << "        if (dialIn(b, n) != -1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 20; }\n";
                break;

            case 906: // The Resonance (boss)
                out << "    {\n";
                out << "        vector<int> b = {4, 5, 6, 7, 0, 1, 2};\n";
                out << "        if (dialIn(b, 0) != 0) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {3, 4, 5, 1, 2};\n";
                out << "        if (dialIn(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {1, 2, 3, 4, 5};\n";
                out << "        if (dialIn(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {2, 1};\n";
                out << "        if (dialIn(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {5};\n";
                out << "        if (dialIn(b, 0) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> b = {5, 1, 2, 3, 4};\n";
                out << "        if (dialIn(b, 0) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> b(n);\n";
                out << "        int rot = n / 3;\n";
                out << "        for (int i = 0; i < n; i++) b[i] = (i + rot) % n;\n";
                out << "        // Minimum is 0\n";
                out << "        if (dialIn(b, 0) != 0) all_correct = false;\n";
                out << "        // No rotation — minimum is first element\n";
                out << "        for (int i = 0; i < n; i++) b[i] = i + 1;\n";
                out << "        if (dialIn(b, 0) != 1) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 17; }\n";
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

        out << "const { dialIn } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 901: // Lock the Frequency
                out << "{\n";
                out << "    let b = [1, 3, 5, 7, 9];\n";
                out << "    if (dialIn(b, 5) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 3, 5, 7, 9];\n";
                out << "    if (dialIn(b, 4) !== -1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1];\n";
                out << "    if (dialIn(b, 1) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [2, 4, 6];\n";
                out << "    if (dialIn(b, 6) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [2, 4, 6];\n";
                out << "    if (dialIn(b, 2) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 2, 3, 4, 5];\n";
                out << "    if (dialIn(b, 0) !== -1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n);\n";
                out << "    for (let i = 0; i < n; i++) b[i] = i * 2;\n";
                out << "    // Search for last element\n";
                out << "    if (dialIn(b, (n - 1) * 2) !== n - 1) allCorrect = false;\n";
                out << "    // Search for missing element\n";
                out << "    if (dialIn(b, 1) !== -1) allCorrect = false;\n";
                out << "    // Search for first element\n";
                out << "    if (dialIn(b, 0) !== 0) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 17; }\n";
                break;

            case 902: // Lower Bound
                out << "{\n";
                out << "    let b = [1, 3, 3, 5, 7];\n";
                out << "    if (dialIn(b, 3) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 3, 5, 7];\n";
                out << "    if (dialIn(b, 4) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 3, 5, 7];\n";
                out << "    if (dialIn(b, 8) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 3, 5, 7];\n";
                out << "    if (dialIn(b, 0) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [5, 5, 5, 5];\n";
                out << "    if (dialIn(b, 5) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1];\n";
                out << "    if (dialIn(b, 1) !== 0) allCorrect = false;\n";
                out << "    if (dialIn(b, 2) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n);\n";
                out << "    for (let i = 0; i < n; i++) b[i] = i * 2; // 0,2,4,...\n";
                out << "    // Lower bound of 1 -> index 1 (first val >= 1 is 2 at index 1)\n";
                out << "    if (dialIn(b, 1) !== 1) allCorrect = false;\n";
                out << "    // Lower bound of 0 -> index 0\n";
                out << "    if (dialIn(b, 0) !== 0) allCorrect = false;\n";
                out << "    // Lower bound beyond max -> n\n";
                out << "    if (dialIn(b, n * 2) !== n) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 18; }\n";
                break;

            case 903: // Count in Range
                out << "{\n";
                out << "    let b = [3, 1, 2, 3, 4, 5, 6, 7];\n";
                out << "    if (dialIn(b, 2) !== 4) allCorrect = false; // [2,5] -> 2,3,4,5\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [0, 1, 3, 5, 7];\n";
                out << "    if (dialIn(b, 3) !== 1) allCorrect = false; // [3,3] -> just 3\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [10, 1, 2, 3];\n";
                out << "    if (dialIn(b, 5) !== 0) allCorrect = false; // [5,15] -> nothing\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [100, 1, 2, 3, 4, 5];\n";
                out << "    if (dialIn(b, 1) !== 5) allCorrect = false; // [1,101] -> all 5\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [0, 5, 5, 5, 5];\n";
                out << "    if (dialIn(b, 5) !== 4) allCorrect = false; // [5,5] -> all four 5s\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n + 1);\n";
                out << "    b[0] = 100; // range width\n";
                out << "    for (let i = 1; i <= n; i++) b[i] = i;\n";
                out << "    // [500, 600] — count elements in that range\n";
                out << "    let lo = 500, hi = 600;\n";
                out << "    let expected = Math.min(hi, n) - lo + 1;\n";
                out << "    if (expected < 0) expected = 0;\n";
                out << "    if (dialIn(b, lo) !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 904: // Peak Frequency
                out << "{\n";
                out << "    let b = [1, 3, 5, 4, 2];\n";
                out << "    if (dialIn(b, 0) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 5, 3];\n";
                out << "    if (dialIn(b, 0) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [10, 8, 6, 4, 2];\n";
                out << "    if (dialIn(b, 0) !== 10) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 2, 3, 4, 5];\n";
                out << "    if (dialIn(b, 0) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 10, 2];\n";
                out << "    if (dialIn(b, 0) !== 10) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [42];\n";
                out << "    if (dialIn(b, 0) !== 42) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n);\n";
                out << "    let peak = Math.floor(n / 3);\n";
                out << "    for (let i = 0; i <= peak; i++) b[i] = i;\n";
                out << "    for (let i = peak + 1; i < n; i++) b[i] = peak - (i - peak);\n";
                out << "    if (dialIn(b, 0) !== peak) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 17; }\n";
                break;

            case 905: // Search the Rotation
                out << "{\n";
                out << "    let b = [4, 5, 6, 7, 0, 1, 2];\n";
                out << "    if (dialIn(b, 0) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [4, 5, 6, 7, 0, 1, 2];\n";
                out << "    if (dialIn(b, 3) !== -1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1];\n";
                out << "    if (dialIn(b, 1) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [3, 1, 2];\n";
                out << "    if (dialIn(b, 2) !== 2) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 2, 3, 4, 5];\n";
                out << "    if (dialIn(b, 5) !== 4) allCorrect = false; // no rotation\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [5, 1, 2, 3, 4];\n";
                out << "    if (dialIn(b, 5) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [2, 3, 4, 5, 1];\n";
                out << "    if (dialIn(b, 1) !== 4) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n);\n";
                out << "    let rot = Math.floor(n / 3);\n";
                out << "    // Rotated: [rot, rot+1, ..., n-1, 0, 1, ..., rot-1]\n";
                out << "    for (let i = 0; i < n; i++) b[i] = (i + rot) % n;\n";
                out << "    // Search for 0 — should be at position n - rot\n";
                out << "    if (dialIn(b, 0) !== n - rot) allCorrect = false;\n";
                out << "    // Search for n-1 — should be at position n - rot - 1\n";
                out << "    if (dialIn(b, n - 1) !== n - rot - 1) allCorrect = false;\n";
                out << "    // Search for n — not present\n";
                out << "    if (dialIn(b, n) !== -1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 20; }\n";
                break;

            case 906: // The Resonance (boss)
                out << "{\n";
                out << "    let b = [4, 5, 6, 7, 0, 1, 2];\n";
                out << "    if (dialIn(b, 0) !== 0) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [3, 4, 5, 1, 2];\n";
                out << "    if (dialIn(b, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [1, 2, 3, 4, 5];\n";
                out << "    if (dialIn(b, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [2, 1];\n";
                out << "    if (dialIn(b, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [5];\n";
                out << "    if (dialIn(b, 0) !== 5) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    let b = [5, 1, 2, 3, 4];\n";
                out << "    if (dialIn(b, 0) !== 1) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    let n = " << wave.n << ";\n";
                out << "    let b = new Array(n);\n";
                out << "    let rot = Math.floor(n / 3);\n";
                out << "    for (let i = 0; i < n; i++) b[i] = (i + rot) % n;\n";
                out << "    // Minimum is 0\n";
                out << "    if (dialIn(b, 0) !== 0) allCorrect = false;\n";
                out << "    // No rotation — minimum is first element\n";
                out << "    for (let i = 0; i < n; i++) b[i] = i + 1;\n";
                out << "    if (dialIn(b, 0) !== 1) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 17; }\n";
                break;
        }

        out << "\nconst end = process.hrtime.bigint();\n";
        out << "const ms = Number((end - start) / 1000000n);\n";
        out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
        out << "process.exit(allCorrect ? 0 : 1);\n";

        out.close();
    }
}
