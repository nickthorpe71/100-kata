#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> threadwalker::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        101, "Count the Vibrations", WORLD_NAME, WORLD_DESC,
        "The spider feels a specific tension value — count how many points on the thread match it.",
        "Example: [3,1,4,1,5,1], target=1 -> 3\nExample: [7,7,7], target=7 -> 3\nExample: [1,2,3], target=5 -> 0",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Use a simple for loop to scan every element, incrementing a counter each time it matches target. O(n) time, O(1) space. The key insight is that a single linear pass is both necessary and sufficient.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        102, "Locate the Pulse", WORLD_NAME, WORLD_DESC,
        "Find the first position where the thread matches the target tension. Return -1 if not found.",
        "Example: [3,1,4,1,5], target=4 -> 2\nExample: [1,1,1], target=1 -> 0\nExample: [1,2,3], target=5 -> -1",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Iterate with a for loop and return the index on the first match, or -1 if you reach the end without finding it. O(n) time, O(1) space. No sorting or extra structure needed — just stop as soon as you find target.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        103, "Cut the Frayed Points", WORLD_NAME, WORLD_DESC,
        "Remove all points matching target tension in-place. Return the new length. Order of remaining elements must be preserved.",
        "Example: [3,1,4,1,5], target=1 -> 3 (thread becomes [3,4,5,...])\nExample: [1,1,1], target=1 -> 0\nExample: [1,2,3], target=5 -> 3",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Use a two-pointer / write-pointer technique. A read pointer scans forward; a write pointer tracks where to place the next non-target value. When the read pointer finds a keeper, copy it to the write position and advance both. O(n) time, O(1) space.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        104, "Partition the Thread", WORLD_NAME, WORLD_DESC,
        "Rearrange the thread so all values <= target come before all values > target. Return the partition index (count of elements <= target). Relative order within each side does not matter.",
        "Example: [3,1,4,1,5,9,2,6], target=4 -> 5 (thread has five values <= 4 at front)\nExample: [1,2,3], target=5 -> 3\nExample: [5,5,5], target=3 -> 0",
        "1 <= N <= 200,000",
        200000, 1000, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Use a Lomuto or Hoare partition scheme. Maintain a boundary pointer — swap elements <= target to the front as you scan. After one pass, everything left of the boundary is <= target and everything right is > target. O(n) time, O(1) space.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        105, "Sort the Thread", WORLD_NAME, WORLD_DESC,
        "Sort the entire thread ascending. Return 0. Target is unused — the spider must order everything.",
        "Example: [3,1,4,1,5,9,2,6], target=0 -> 0 (thread becomes [1,1,2,3,4,5,6,9])\nExample: [5,3,1], target=0 -> 0 (thread becomes [1,3,5])",
        "1 <= N <= 200,000",
        200000, 2000, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Implement quicksort or any O(n log n) comparison sort. Pick a pivot, partition the array around it, then recursively sort both halves. The key insight is divide-and-conquer: each partition places one element in its final position and splits the remaining work in half on average.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        106, "Find the Kth Weakest", WORLD_NAME, WORLD_DESC,
        "Find the kth smallest tension value (target=k, 0-indexed). Don't fully sort — be faster than that.",
        "Example: [3,1,4,1,5], target=0 -> 1 (smallest)\nExample: [3,1,4,1,5], target=2 -> 3\nExample: [7], target=0 -> 7",
        "1 <= N <= 500,000\n0 <= target < N\nMust be faster than O(n log n) on average.",
        500000, 500, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Use quickselect — partition the array, then only recurse into the side that contains index k. Unlike full sorting, you discard half the data each step. O(n) average time, O(1) extra space. The key insight is that you never need to sort the side that doesn't contain k.", generateStub, generateRunner, {}, "", ""
    });

    waves.push_back({
        107, "Spin the Thread", WORLD_NAME, WORLD_DESC,
        "Rotate the thread right by target positions in-place. Return the new first element. The spider's anchor has drifted.",
        "Example: [1,2,3,4,5], target=2 -> 4 (thread becomes [4,5,1,2,3])\nExample: [1,2,3], target=0 -> 1\nExample: [1,2,3], target=3 -> 1",
        "1 <= N <= 500,000\n0 <= target",
        500000, 500, 300,
        "int walkThread(vector<int>& thread, int target)",
        "Use the triple reversal trick: reverse [0..n-k-1], reverse [n-k..n-1], then reverse the entire array. This rotates right by k positions in O(n) time and O(1) space. The key insight is that two partial reverses followed by a full reverse is equivalent to a rotation.", generateStub, generateRunner, {}, "", ""
    });

    quizbank::attachThemedQuiz(
        waves,
        "an array traversal / in-place manipulation approach",
        "Need direct index-based access for scanning, partitioning, or rearranging elements",
        "O(1) space"
    );
    waves[0].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, return how many segments have tension exactly equal to `target`.";
    waves[0].flavor_text = "A two-legged spider clings to a single strand over a black drop, reading tremors through the silk with its front limbs. Tonight it is hunting one precise vibration pattern and must count every segment that hums with the same tension as the signal in its mind.";
    waves[1].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, return the first index whose tension equals `target`, or `-1` if the target tension never appears.";
    waves[1].flavor_text = "A two-legged spider lowers its face to the thread and listens. The earliest matching pulse could mean prey crossing the line, and if it reacts too late the entire strand will have lied to it.";
    waves[2].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, remove every segment whose tension equals `target` in place. Preserve the order of the remaining values and return the new logical length.";
    waves[2].flavor_text = "A two-legged spider tests its lifeline and feels weak, frayed sections pulsing with the wrong tension. It cannot spin a new thread now; it must slide the healthy segments forward and cut away the bad ones without disturbing the surviving pattern.";
    waves[3].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, rearrange the array so every value less than or equal to `target` appears before every value greater than `target`. Return the number of values in the left partition.";
    waves[3].flavor_text = "A two-legged spider braces against a coming gust and rebalances its line. Safe segments must be gathered to one side of the strand while overloaded ones are pushed away, leaving a clean boundary between what can hold and what will snap.";
    waves[4].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, sort the array in ascending order in place. Return `0` after the array has been sorted.";
    waves[4].flavor_text = "A two-legged spider kneels on the silk and arranges every pull it can feel, from the faintest whisper to the sharpest strain. Only a perfectly ordered strand will let it read the night without confusion.";
    waves[5].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, return the `k`th smallest tension value, where `k = target` is zero-indexed. Do not fully sort the array.";
    waves[5].flavor_text = "A two-legged spider senses hundreds of tiny stresses racing through its only bridge and needs the exact weakness threshold that separates sound silk from failing silk. It does not have time to rank every segment; it only needs the one critical order statistic.";
    waves[6].clear_prompt = "Given an array `thread` where `thread[i]` is the tension at segment `i`, rotate the array to the right by `target` positions in place and return the new first element.";
    waves[6].flavor_text = "A two-legged spider has lost its anchor in a gust and the whole strand has twisted around the void. The pattern is still there, only shifted. If it can rotate the thread back into alignment, the first vibration it feels will tell it whether the web can still be trusted.";
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Triple-reversal in-place rotation",
            "Heap-based reorder of elements",
            "Sliding window on the rotated view",
            "Union-Find over cyclic positions"
        },
        0,
        "Need direct index-based access for scanning, partitioning, or rearranging elements",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    return waves;
}

void threadwalker::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int walkThread(vector<int>& thread, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 101: // Count the Vibrations
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5, 1};\n";
            out << "        if (walkThread(t, 1) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {7, 7, 7};\n";
            out << "        if (walkThread(t, 7) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        if (walkThread(t, 5) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {42};\n";
            out << "        if (walkThread(t, 42) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = i % 10;\n";
            out << "        if (walkThread(t, 0) != n / 10) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 102: // Locate the Pulse
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5};\n";
            out << "        if (walkThread(t, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 1, 1};\n";
            out << "        if (walkThread(t, 1) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        if (walkThread(t, 5) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {5, 3, 5, 3, 5};\n";
            out << "        if (walkThread(t, 3) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n, 0);\n";
            out << "        t[n - 1] = 999;\n";
            out << "        if (walkThread(t, 999) != n - 1) all_correct = false;\n";
            out << "        if (walkThread(t, 888) != -1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 103: // Cut the Frayed Points
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5};\n";
            out << "        int len = walkThread(t, 1);\n";
            out << "        if (len != 3 || t[0] != 3 || t[1] != 4 || t[2] != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 1, 1};\n";
            out << "        int len = walkThread(t, 1);\n";
            out << "        if (len != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        int len = walkThread(t, 5);\n";
            out << "        if (len != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {4, 1, 4, 2, 4};\n";
            out << "        int len = walkThread(t, 4);\n";
            out << "        if (len != 2 || t[0] != 1 || t[1] != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = i % 3;\n";
            out << "        // Remove all 0s — should leave 2/3 of elements\n";
            out << "        int len = walkThread(t, 0);\n";
            out << "        int expected = n - (n + 2) / 3;\n";
            out << "        if (len != expected) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 104: // Partition the Thread
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5, 9, 2, 6};\n";
            out << "        int p = walkThread(t, 4);\n";
            out << "        if (p != 5) all_correct = false;\n";
            out << "        for (int i = 0; i < p; i++) if (t[i] > 4) all_correct = false;\n";
            out << "        for (int i = p; i < (int)t.size(); i++) if (t[i] <= 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        int p = walkThread(t, 5);\n";
            out << "        if (p != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {5, 5, 5};\n";
            out << "        int p = walkThread(t, 3);\n";
            out << "        if (p != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {5, 5, 5};\n";
            out << "        int p = walkThread(t, 5);\n";
            out << "        if (p != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = n - i;\n";
            out << "        int mid = n / 2;\n";
            out << "        int p = walkThread(t, mid);\n";
            out << "        if (p != mid) all_correct = false;\n";
            out << "        for (int i = 0; i < p; i++) if (t[i] > mid) all_correct = false;\n";
            out << "        for (int i = p; i < n; i++) if (t[i] <= mid) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 19; }\n";
            break;

        case 105: // Sort the Thread
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5, 9, 2, 6};\n";
            out << "        walkThread(t, 0);\n";
            out << "        vector<int> expected = {1, 1, 2, 3, 4, 5, 6, 9};\n";
            out << "        if (t != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {5, 3, 1};\n";
            out << "        walkThread(t, 0);\n";
            out << "        vector<int> expected = {1, 3, 5};\n";
            out << "        if (t != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1};\n";
            out << "        walkThread(t, 0);\n";
            out << "        if (t[0] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 1, 1, 1};\n";
            out << "        walkThread(t, 0);\n";
            out << "        for (int x : t) if (x != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = n - i;\n";
            out << "        walkThread(t, 0);\n";
            out << "        for (int i = 1; i < n; i++) if (t[i] < t[i-1]) { all_correct = false; break; }\n";
            out << "        if (t[0] != 1 || t[n-1] != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 106: // Find the Kth Weakest
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5};\n";
            out << "        if (walkThread(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5};\n";
            out << "        if (walkThread(t, 2) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {3, 1, 4, 1, 5};\n";
            out << "        if (walkThread(t, 4) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {7};\n";
            out << "        if (walkThread(t, 0) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {2, 2, 2, 2};\n";
            out << "        if (walkThread(t, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = n - i; // [n, n-1, ..., 1]\n";
            out << "        // k=0 should give 1, k=n-1 should give n\n";
            out << "        if (walkThread(t, 0) != 1) all_correct = false;\n";
            out << "        // Reset and test median\n";
            out << "        for (int i = 0; i < n; i++) t[i] = n - i;\n";
            out << "        if (walkThread(t, n/2) != n/2 + 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 107: // Spin the Thread
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3, 4, 5};\n";
            out << "        int r = walkThread(t, 2);\n";
            out << "        vector<int> expected = {4, 5, 1, 2, 3};\n";
            out << "        if (t != expected || r != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        int r = walkThread(t, 0);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (t != expected || r != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {1, 2, 3};\n";
            out << "        int r = walkThread(t, 3);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (t != expected || r != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> t = {10, 20};\n";
            out << "        int r = walkThread(t, 1);\n";
            out << "        vector<int> expected = {20, 10};\n";
            out << "        if (t != expected || r != 20) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> t(n);\n";
            out << "        for (int i = 0; i < n; i++) t[i] = i + 1;\n";
            out << "        int k = n / 3;\n";
            out << "        int r = walkThread(t, k);\n";
            out << "        if (r != n - k + 1) all_correct = false;\n";
            out << "        if (t[0] != n - k + 1 || t[k] != 1) all_correct = false;\n";
            out << "        // Rotate back\n";
            out << "        walkThread(t, n - k);\n";
            out << "        if (t[0] != 1 || t[n - 1] != n) all_correct = false;\n";
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

    out << "const { walkThread } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 101: // Count the Vibrations
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5, 1];\n";
            out << "    if (walkThread(t, 1) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [7, 7, 7];\n";
            out << "    if (walkThread(t, 7) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    if (walkThread(t, 5) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [42];\n";
            out << "    if (walkThread(t, 42) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = i % 10;\n";
            out << "    if (walkThread(t, 0) !== Math.floor(n / 10)) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 102: // Locate the Pulse
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5];\n";
            out << "    if (walkThread(t, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 1, 1];\n";
            out << "    if (walkThread(t, 1) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    if (walkThread(t, 5) !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [5, 3, 5, 3, 5];\n";
            out << "    if (walkThread(t, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n).fill(0);\n";
            out << "    t[n - 1] = 999;\n";
            out << "    if (walkThread(t, 999) !== n - 1) allCorrect = false;\n";
            out << "    if (walkThread(t, 888) !== -1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 103: // Cut the Frayed Points
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5];\n";
            out << "    let len = walkThread(t, 1);\n";
            out << "    if (len !== 3 || t[0] !== 3 || t[1] !== 4 || t[2] !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 1, 1];\n";
            out << "    let len = walkThread(t, 1);\n";
            out << "    if (len !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    let len = walkThread(t, 5);\n";
            out << "    if (len !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [4, 1, 4, 2, 4];\n";
            out << "    let len = walkThread(t, 4);\n";
            out << "    if (len !== 2 || t[0] !== 1 || t[1] !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = i % 3;\n";
            out << "    let len = walkThread(t, 0);\n";
            out << "    let expected = n - Math.floor((n + 2) / 3);\n";
            out << "    if (len !== expected) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 104: // Partition the Thread
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5, 9, 2, 6];\n";
            out << "    let p = walkThread(t, 4);\n";
            out << "    if (p !== 5) allCorrect = false;\n";
            out << "    for (let i = 0; i < p; i++) if (t[i] > 4) allCorrect = false;\n";
            out << "    for (let i = p; i < t.length; i++) if (t[i] <= 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    let p = walkThread(t, 5);\n";
            out << "    if (p !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [5, 5, 5];\n";
            out << "    let p = walkThread(t, 3);\n";
            out << "    if (p !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [5, 5, 5];\n";
            out << "    let p = walkThread(t, 5);\n";
            out << "    if (p !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = n - i;\n";
            out << "    let mid = Math.floor(n / 2);\n";
            out << "    let p = walkThread(t, mid);\n";
            out << "    if (p !== mid) allCorrect = false;\n";
            out << "    for (let i = 0; i < p; i++) if (t[i] > mid) allCorrect = false;\n";
            out << "    for (let i = p; i < n; i++) if (t[i] <= mid) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 19; }\n";
            break;

        case 105: // Sort the Thread
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5, 9, 2, 6];\n";
            out << "    walkThread(t, 0);\n";
            out << "    let expected = [1, 1, 2, 3, 4, 5, 6, 9];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [5, 3, 1];\n";
            out << "    walkThread(t, 0);\n";
            out << "    let expected = [1, 3, 5];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1];\n";
            out << "    walkThread(t, 0);\n";
            out << "    if (t[0] !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 1, 1, 1];\n";
            out << "    walkThread(t, 0);\n";
            out << "    for (let x of t) if (x !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = n - i;\n";
            out << "    walkThread(t, 0);\n";
            out << "    for (let i = 1; i < n; i++) if (t[i] < t[i-1]) { allCorrect = false; break; }\n";
            out << "    if (t[0] !== 1 || t[n-1] !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 106: // Find the Kth Weakest
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5];\n";
            out << "    if (walkThread(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5];\n";
            out << "    if (walkThread(t, 2) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [3, 1, 4, 1, 5];\n";
            out << "    if (walkThread(t, 4) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [7];\n";
            out << "    if (walkThread(t, 0) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [2, 2, 2, 2];\n";
            out << "    if (walkThread(t, 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = n - i;\n";
            out << "    if (walkThread(t, 0) !== 1) allCorrect = false;\n";
            out << "    for (let i = 0; i < n; i++) t[i] = n - i;\n";
            out << "    if (walkThread(t, Math.floor(n / 2)) !== Math.floor(n / 2) + 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 107: // Spin the Thread
            out << "{\n";
            out << "    let t = [1, 2, 3, 4, 5];\n";
            out << "    let r = walkThread(t, 2);\n";
            out << "    let expected = [4, 5, 1, 2, 3];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected) || r !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    let r = walkThread(t, 0);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected) || r !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [1, 2, 3];\n";
            out << "    let r = walkThread(t, 3);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected) || r !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = [10, 20];\n";
            out << "    let r = walkThread(t, 1);\n";
            out << "    let expected = [20, 10];\n";
            out << "    if (JSON.stringify(t) !== JSON.stringify(expected) || r !== 20) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) t[i] = i + 1;\n";
            out << "    let k = Math.floor(n / 3);\n";
            out << "    let r = walkThread(t, k);\n";
            out << "    if (r !== n - k + 1) allCorrect = false;\n";
            out << "    if (t[0] !== n - k + 1 || t[k] !== 1) allCorrect = false;\n";
            out << "    walkThread(t, n - k);\n";
            out << "    if (t[0] !== 1 || t[n - 1] !== n) allCorrect = false;\n";
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
