#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> hanging_court::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        401, "Weigh the Pair", WORLD_NAME, WORLD_DESC,
        "Check if the accused are already sorted ascending. Return 1 if sorted, 0 otherwise.",
        "Example: [1,2,3,4,5] -> 1\nExample: [1,3,2,4,5] -> 0\nExample: [5] -> 1\nExample: [1,1,1] -> 1",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int passJudgment(vector<int>& accused, int target)",
        "A single pass comparing adjacent elements. If any a[i] > a[i+1], the array is not sorted and you return 0 immediately. If you reach the end without finding such a pair, return 1. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        402, "Bubble the Guilty", WORLD_NAME, WORLD_DESC,
        "Sort the accused ascending using bubble sort. Return the total number of swaps performed.",
        "Example: [3,1,2] -> 2 (swap 3&1, swap 3&2, result [1,2,3])\nExample: [1,2,3] -> 0\nExample: [2,1] -> 1",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 5000, 300,
        "int passJudgment(vector<int>& accused, int target)",
        "Nested loops: the outer loop counts passes, the inner loop compares adjacent elements and swaps when out of order. Count every swap performed. O(n^2) time, O(1) space. An optimization is to stop early if a full pass produces no swaps.", generateStub, generateRunner
    });

    waves.push_back({
        403, "Insert the Accused", WORLD_NAME, WORLD_DESC,
        "Sort using insertion sort. Return 0. For each element, shift larger elements right and insert into correct position.",
        "Example: [3,1,2] -> 0 (accused becomes [1,2,3])\nExample: [5,4,3,2,1] -> 0 (accused becomes [1,2,3,4,5])",
        "1 <= N <= 10,000\ntarget is unused.",
        10000, 5000, 300,
        "int passJudgment(vector<int>& accused, int target)",
        "For each element starting from index 1, shift all larger elements to the right to make room, then insert the current element in its correct sorted position. O(n^2) worst case but very efficient for nearly-sorted data since few shifts are needed.", generateStub, generateRunner
    });

    waves.push_back({
        404, "Split the Court", WORLD_NAME, WORLD_DESC,
        "Sort using merge sort. Return 0. Divide the array, sort each half, merge them back.",
        "Example: [5,2,8,1,9] -> 0 (accused becomes [1,2,5,8,9])\nExample: [1] -> 0",
        "1 <= N <= 500,000\ntarget is unused.",
        500000, 2000, 300,
        "int passJudgment(vector<int>& accused, int target)",
        "Divide the array in half, recursively sort each half, then merge the two sorted halves using a temporary buffer. The merge step walks two pointers through the sorted halves, always picking the smaller element. O(n log n) time, O(n) extra space.", generateStub, generateRunner
    });

    waves.push_back({
        405, "The Quick Verdict", WORLD_NAME, WORLD_DESC,
        "Sort using quicksort. Return 0. Pick a pivot, partition around it, recurse on both halves.",
        "Example: [3,6,8,10,1,2,1] -> 0 (accused becomes [1,1,2,3,6,8,10])\nExample: [1,1,1] -> 0",
        "1 <= N <= 500,000\ntarget is unused.",
        500000, 2000, 300,
        "int passJudgment(vector<int>& accused, int target)",
        "Pick a pivot (median-of-three helps avoid worst case), partition the array so elements <= pivot are left and > pivot are right, then recurse on both sides. O(n log n) average, O(n^2) worst case. In-place partitioning keeps space usage to O(log n) stack frames.", generateStub, generateRunner
    });

    waves.push_back({
        406, "Merge the Dockets", WORLD_NAME, WORLD_DESC,
        "Two sorted dockets. accused has m real elements followed by enough zero-padding for docket's n elements. Merge docket into accused in-place so the result is sorted. target = m. Return 0. Parameter docket is added this wave.",
        "Example: accused=[1,3,5,0,0], docket=[2,4], target=3 -> 0 (accused becomes [1,2,3,4,5])\nExample: accused=[0,0,0], docket=[1,2,3], target=0 -> 0 (accused becomes [1,2,3])",
        "0 <= m, n <= 200,000\nBoth sorted ascending.",
        200000, 1000, 300,
        "int passJudgment(vector<int>& accused, vector<int>& docket, int target)",
        "Start two pointers at the ends of both real (sorted) sections and write from the back of the padded array forward. At each step, place the larger of the two pointed-to elements at the write position. O(n+m) time, O(1) extra space since you fill the padding in-place.", generateStub, generateRunner
    });

    waves.push_back({
        407, "The Executioner", WORLD_NAME, WORLD_DESC,
        "Find the kth smallest guilt value (target=k, 0-indexed) without fully sorting. Return that value. Must be O(n) average — use quickselect (partition + recurse on one side only).",
        "Example: [3,1,4,1,5], target=0 -> 1\nExample: [3,1,4,1,5], target=2 -> 3\nExample: [7], target=0 -> 7",
        "1 <= N <= 500,000\n0 <= target < N\nMust be faster than O(n log n) on average.\ndocket is empty.",
        500000, 500, 300,
        "int passJudgment(vector<int>& accused, vector<int>& docket, int target)",
        "Use quickselect: partition the array, check where the pivot lands relative to k. Only recurse into the side that contains index k, discarding the other half. O(n) average time, O(n^2) worst case. The key insight is that you never need to fully sort — just narrow down which partition contains the kth element.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a sorting / partitioning approach",
        "Need global ordering or pivot-based partitioning to make progress efficiently",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Quickselect partitioning around a pivot",
            "Breadth-first traversal",
            "Dynamic programming over prefixes",
            "Heap of every element then full extraction"
        },
        0,
        "Need global ordering or pivot-based partitioning to make progress efficiently",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given an array `accused`, return `1` if it is already sorted in ascending order and `0` otherwise.";
    waves[0].flavor_text = "In the Hanging Court, the chief clerk inspects the docket before judgment begins. If the names are already ordered, the ritual may proceed; if not, disorder itself stands accused.";
    waves[1].clear_prompt = "Given an array `accused`, sort it with bubble sort and return the total number of swaps performed.";
    waves[1].flavor_text = "Some clerks in the court trust only the oldest method: compare neighbors, trade places, and repeat until no inversion remains. The chief clerk counts every swap like a tiny confession.";
    waves[2].clear_prompt = "Given an array `accused`, sort it using insertion sort in place and return `0`.";
    waves[2].flavor_text = "Each new accusation must be inserted into the record exactly where it belongs. The chief clerk shifts the older entries aside until the line reads in proper order again.";
    waves[3].clear_prompt = "Given an array `accused`, sort it using merge sort and return `0`.";
    waves[3].flavor_text = "The court breaks a case into smaller hearings, resolves them separately, then stitches the sorted verdicts back together. The chief clerk oversees the merge with cold patience.";
    waves[4].clear_prompt = "Given an array `accused`, sort it using quicksort and return `0`.";
    waves[4].flavor_text = "Impatient judges prefer swifter methods. The chief clerk chooses a pivot, throws lesser cases to one side and greater ones to the other, and lets the partitions pass sentence.";
    waves[5].clear_prompt = "Given sorted arrays `accused` and `docket` as encoded by the wave, merge them in place into `accused` and return `0`.";
    waves[5].flavor_text = "Two clerks arrive with already-ordered records. The chief clerk must weave them into a single ledger without spilling either stack into the dark.";
    waves[6].clear_prompt = "Given an unsorted array `accused` and zero-indexed `target = k`, return the `k`th smallest value without fully sorting the array.";
    waves[6].flavor_text = "Execution does not require a perfectly ordered court roll, only the one name at the required rank. The chief clerk narrows the field around pivots until that position stands exposed.";
    return waves;
}

void hanging_court::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    if (wave.id <= 405) {
        out << "int passJudgment(vector<int>& accused, int target);\n\n";
    } else {
        out << "int passJudgment(vector<int>& accused, vector<int>& docket, int target);\n\n";
    }

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 401: // Weigh the Pair
            out << "    {\n";
            out << "        vector<int> a = {1, 2, 3, 4, 5};\n";
            out << "        if (passJudgment(a, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1, 3, 2, 4, 5};\n";
            out << "        if (passJudgment(a, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {5};\n";
            out << "        if (passJudgment(a, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1, 1, 1};\n";
            out << "        if (passJudgment(a, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {5, 4, 3, 2, 1};\n";
            out << "        if (passJudgment(a, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n);\n";
            out << "        for (int i = 0; i < n; i++) a[i] = i;\n";
            out << "        if (passJudgment(a, 0) != 1) all_correct = false;\n";
            out << "        a[n / 2] = -1;\n";
            out << "        if (passJudgment(a, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 402: // Bubble the Guilty
            out << "    {\n";
            out << "        vector<int> a = {3, 1, 2};\n";
            out << "        int swaps = passJudgment(a, 0);\n";
            out << "        if (a[0] != 1 || a[1] != 2 || a[2] != 3) all_correct = false;\n";
            out << "        if (swaps != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1, 2, 3};\n";
            out << "        int swaps = passJudgment(a, 0);\n";
            out << "        if (swaps != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {2, 1};\n";
            out << "        int swaps = passJudgment(a, 0);\n";
            out << "        if (a[0] != 1 || a[1] != 2 || swaps != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {5, 4, 3, 2, 1};\n";
            out << "        int swaps = passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 2, 3, 4, 5};\n";
            out << "        if (a != expected || swaps != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n);\n";
            out << "        for (int i = 0; i < n; i++) a[i] = n - i;\n";
            out << "        passJudgment(a, 0);\n";
            out << "        for (int i = 1; i < n; i++) if (a[i] < a[i-1]) { all_correct = false; break; }\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 15; }\n";
            break;

        case 403: // Insert the Accused
            out << "    {\n";
            out << "        vector<int> a = {3, 1, 2};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {5, 4, 3, 2, 1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 2, 3, 4, 5};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        if (a[0] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {2, 2, 1, 1, 3, 3};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 1, 2, 2, 3, 3};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n);\n";
            out << "        for (int i = 0; i < n; i++) a[i] = n - i;\n";
            out << "        passJudgment(a, 0);\n";
            out << "        for (int i = 1; i < n; i++) if (a[i] < a[i-1]) { all_correct = false; break; }\n";
            out << "        if (a[0] != 1 || a[n-1] != n) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 404: // Split the Court (merge sort)
            out << "    {\n";
            out << "        vector<int> a = {5, 2, 8, 1, 9};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 2, 5, 8, 9};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        if (a[0] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {3, 3, 3, 1, 1, 1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 1, 1, 3, 3, 3};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        for (int i = 0; i < 10; i++) if (a[i] != i + 1) { all_correct = false; break; }\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n);\n";
            out << "        for (int i = 0; i < n; i++) a[i] = n - i;\n";
            out << "        passJudgment(a, 0);\n";
            out << "        for (int i = 1; i < n; i++) if (a[i] < a[i-1]) { all_correct = false; break; }\n";
            out << "        if (a[0] != 1 || a[n-1] != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 25; }\n";
            break;

        case 405: // The Quick Verdict (quicksort)
            out << "    {\n";
            out << "        vector<int> a = {3, 6, 8, 10, 1, 2, 1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {1, 1, 2, 3, 6, 8, 10};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1, 1, 1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        if (a[0] != 1 || a[1] != 1 || a[2] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {5, -3, 0, 2, -1, 4};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        vector<int> expected = {-3, -1, 0, 2, 4, 5};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1};\n";
            out << "        passJudgment(a, 0);\n";
            out << "        if (a[0] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n);\n";
            out << "        // Interleaved pattern to avoid worst-case pivot\n";
            out << "        for (int i = 0; i < n; i++) a[i] = (i % 2 == 0) ? i : n - i;\n";
            out << "        passJudgment(a, 0);\n";
            out << "        for (int i = 1; i < n; i++) if (a[i] < a[i-1]) { all_correct = false; break; }\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 406: // Merge the Dockets
            out << "    {\n";
            out << "        vector<int> a = {1, 3, 5, 0, 0}, d = {2, 4};\n";
            out << "        passJudgment(a, d, 3);\n";
            out << "        vector<int> expected = {1, 2, 3, 4, 5};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {0, 0, 0}, d = {1, 2, 3};\n";
            out << "        passJudgment(a, d, 0);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {1, 2, 3}, d = {};\n";
            out << "        passJudgment(a, d, 3);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {4, 5, 6, 0, 0, 0}, d = {1, 2, 3};\n";
            out << "        passJudgment(a, d, 3);\n";
            out << "        vector<int> expected = {1, 2, 3, 4, 5, 6};\n";
            out << "        if (a != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        int m = n / 2;\n";
            out << "        vector<int> a(n, 0), d(n - m);\n";
            out << "        for (int i = 0; i < m; i++) a[i] = i * 2;\n";
            out << "        for (int i = 0; i < n - m; i++) d[i] = i * 2 + 1;\n";
            out << "        passJudgment(a, d, m);\n";
            out << "        for (int i = 0; i < n; i++) if (a[i] != i) { all_correct = false; break; }\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 407: // The Executioner (quickselect)
            out << "    {\n";
            out << "        vector<int> a = {3, 1, 4, 1, 5}, empty;\n";
            out << "        if (passJudgment(a, empty, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {3, 1, 4, 1, 5}, empty;\n";
            out << "        if (passJudgment(a, empty, 2) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {3, 1, 4, 1, 5}, empty;\n";
            out << "        if (passJudgment(a, empty, 4) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {7}, empty;\n";
            out << "        if (passJudgment(a, empty, 0) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {2, 2, 2, 2}, empty;\n";
            out << "        if (passJudgment(a, empty, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> a = {10, -5, 3, 0, 7}, empty;\n";
            out << "        if (passJudgment(a, empty, 1) != 0) all_correct = false; // sorted: -5,0,3,7,10\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> a(n), empty;\n";
            out << "        for (int i = 0; i < n; i++) a[i] = n - i; // [n, n-1, ..., 1]\n";
            out << "        if (passJudgment(a, empty, 0) != 1) all_correct = false;\n";
            out << "        // Reset and find median\n";
            out << "        for (int i = 0; i < n; i++) a[i] = n - i;\n";
            out << "        if (passJudgment(a, empty, n / 2) != n / 2 + 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 19; }\n";
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

    out << "const { passJudgment } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 401: // Weigh the Pair
            out << "{\n";
            out << "    let a = [1, 2, 3, 4, 5];\n";
            out << "    if (passJudgment(a, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1, 3, 2, 4, 5];\n";
            out << "    if (passJudgment(a, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [5];\n";
            out << "    if (passJudgment(a, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1, 1, 1];\n";
            out << "    if (passJudgment(a, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [5, 4, 3, 2, 1];\n";
            out << "    if (passJudgment(a, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(i);\n";
            out << "    if (passJudgment(a, 0) !== 1) allCorrect = false;\n";
            out << "    a[Math.floor(n / 2)] = -1;\n";
            out << "    if (passJudgment(a, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 402: // Bubble the Guilty
            out << "{\n";
            out << "    let a = [3, 1, 2];\n";
            out << "    let swaps = passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1 || a[1] !== 2 || a[2] !== 3) allCorrect = false;\n";
            out << "    if (swaps !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1, 2, 3];\n";
            out << "    let swaps = passJudgment(a, 0);\n";
            out << "    if (swaps !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [2, 1];\n";
            out << "    let swaps = passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1 || a[1] !== 2 || swaps !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [5, 4, 3, 2, 1];\n";
            out << "    let swaps = passJudgment(a, 0);\n";
            out << "    let expected = [1, 2, 3, 4, 5];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected) || swaps !== 10) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(n - i);\n";
            out << "    passJudgment(a, 0);\n";
            out << "    for (let i = 1; i < n; i++) if (a[i] < a[i - 1]) { allCorrect = false; break; }\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 15; }\n";
            break;

        case 403: // Insert the Accused
            out << "{\n";
            out << "    let a = [3, 1, 2];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [5, 4, 3, 2, 1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 2, 3, 4, 5];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [2, 2, 1, 1, 3, 3];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 1, 2, 2, 3, 3];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(n - i);\n";
            out << "    passJudgment(a, 0);\n";
            out << "    for (let i = 1; i < n; i++) if (a[i] < a[i - 1]) { allCorrect = false; break; }\n";
            out << "    if (a[0] !== 1 || a[n - 1] !== n) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 404: // Split the Court (merge sort)
            out << "{\n";
            out << "    let a = [5, 2, 8, 1, 9];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 2, 5, 8, 9];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [3, 3, 3, 1, 1, 1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 1, 1, 3, 3, 3];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    for (let i = 0; i < 10; i++) if (a[i] !== i + 1) { allCorrect = false; break; }\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(n - i);\n";
            out << "    passJudgment(a, 0);\n";
            out << "    for (let i = 1; i < n; i++) if (a[i] < a[i - 1]) { allCorrect = false; break; }\n";
            out << "    if (a[0] !== 1 || a[n - 1] !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 25; }\n";
            break;

        case 405: // The Quick Verdict (quicksort)
            out << "{\n";
            out << "    let a = [3, 6, 8, 10, 1, 2, 1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [1, 1, 2, 3, 6, 8, 10];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1, 1, 1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1 || a[1] !== 1 || a[2] !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [5, -3, 0, 2, -1, 4];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    let expected = [-3, -1, 0, 2, 4, 5];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1];\n";
            out << "    passJudgment(a, 0);\n";
            out << "    if (a[0] !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    // Interleaved pattern to avoid worst-case pivot\n";
            out << "    for (let i = 0; i < n; i++) a.push((i % 2 === 0) ? i : n - i);\n";
            out << "    passJudgment(a, 0);\n";
            out << "    for (let i = 1; i < n; i++) if (a[i] < a[i - 1]) { allCorrect = false; break; }\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 406: // Merge the Dockets (3-param)
            out << "{\n";
            out << "    let a = [1, 3, 5, 0, 0], d = [2, 4];\n";
            out << "    passJudgment(a, d, 3);\n";
            out << "    let expected = [1, 2, 3, 4, 5];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [0, 0, 0], d = [1, 2, 3];\n";
            out << "    passJudgment(a, d, 0);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [1, 2, 3], d = [];\n";
            out << "    passJudgment(a, d, 3);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [4, 5, 6, 0, 0, 0], d = [1, 2, 3];\n";
            out << "    passJudgment(a, d, 3);\n";
            out << "    let expected = [1, 2, 3, 4, 5, 6];\n";
            out << "    if (JSON.stringify(a) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let m = Math.floor(n / 2);\n";
            out << "    let a = new Array(n).fill(0);\n";
            out << "    let d = [];\n";
            out << "    for (let i = 0; i < m; i++) a[i] = i * 2;\n";
            out << "    for (let i = 0; i < n - m; i++) d.push(i * 2 + 1);\n";
            out << "    passJudgment(a, d, m);\n";
            out << "    for (let i = 0; i < n; i++) if (a[i] !== i) { allCorrect = false; break; }\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 407: // The Executioner (quickselect, 3-param, docket is empty)
            out << "{\n";
            out << "    let a = [3, 1, 4, 1, 5];\n";
            out << "    if (passJudgment(a, [], 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [3, 1, 4, 1, 5];\n";
            out << "    if (passJudgment(a, [], 2) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [3, 1, 4, 1, 5];\n";
            out << "    if (passJudgment(a, [], 4) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [7];\n";
            out << "    if (passJudgment(a, [], 0) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [2, 2, 2, 2];\n";
            out << "    if (passJudgment(a, [], 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [10, -5, 3, 0, 7];\n";
            out << "    if (passJudgment(a, [], 1) !== 0) allCorrect = false; // sorted: -5,0,3,7,10\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(n - i); // [n, n-1, ..., 1]\n";
            out << "    if (passJudgment(a, [], 0) !== 1) allCorrect = false;\n";
            out << "    // Reset and find median\n";
            out << "    a = [];\n";
            out << "    for (let i = 0; i < n; i++) a.push(n - i);\n";
            out << "    if (passJudgment(a, [], Math.floor(n / 2)) !== Math.floor(n / 2) + 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 19; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
