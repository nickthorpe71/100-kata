#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> closing_jaws::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        301, "Find the Pair", WORLD_NAME, WORLD_DESC,
        "The corridor is sorted. Find two elements that sum to target. Return 1 if a pair exists, 0 otherwise.",
        "Example: [1,2,3,4,6], target=6 -> 1 (2+4)\nExample: [1,2,3], target=7 -> 0\nExample: [1,5], target=6 -> 1",
        "1 <= N <= 100,000\nCorridor is sorted ascending.",
        100000, 1000, 300,
        "int closeJaws(vector<int>& corridor, int target)",
        "Use two pointers starting from both ends of the sorted array. If the sum is less than target, advance the left pointer. If greater, retreat the right pointer. If equal, you found the pair. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        302, "Count the Pairs", WORLD_NAME, WORLD_DESC,
        "Count all unique pairs of elements that sum to target. Each pair uses distinct indices. Skip duplicate pairs.",
        "Example: [1,2,3,4,5], target=6 -> 2 (1+5, 2+4)\nExample: [1,1,1,1], target=2 -> 1 (only one unique pair value)\nExample: [1,2,3], target=10 -> 0",
        "1 <= N <= 200,000\nCorridor is sorted ascending.",
        200000, 1000, 300,
        "int closeJaws(vector<int>& corridor, int target)",
        "Same two-pointer approach from both ends, but count matches instead of returning early. When a pair sums to target, increment the count and skip duplicate values on both sides before continuing. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        303, "The Squeeze", WORLD_NAME, WORLD_DESC,
        "Find the pair whose sum is closest to target. Return that sum. If two sums are equally close, return either.",
        "Example: [1,2,3,4,5], target=8 -> 8 (3+5)\nExample: [1,2,3,4,5], target=100 -> 9 (4+5)\nExample: [-1,2,4,8], target=5 -> 6 (2+4)",
        "2 <= N <= 200,000\nCorridor is sorted ascending.",
        200000, 1000, 300,
        "int closeJaws(vector<int>& corridor, int target)",
        "Two pointers from both ends, tracking the closest sum seen so far. At each step, compute the current pair sum and update the best if abs(sum - target) improves. Move the left pointer right if sum < target, otherwise move the right pointer left. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        304, "Triple Hunt", WORLD_NAME, WORLD_DESC,
        "Find three elements that sum to target. Return 1 if a triplet exists, 0 otherwise.",
        "Example: [-1,0,1,2,-1,-4], target=0 -> 1 (-1+0+1)\nExample: [1,2,3], target=7 -> 0\nExample: [0,0,0], target=0 -> 1",
        "1 <= N <= 10,000\nCorridor is sorted ascending.",
        10000, 2000, 300,
        "int closeJaws(vector<int>& corridor, int target)",
        "Sort the array first, then for each element, run a two-pointer search on the remaining elements to find a pair that sums to (target - current). The outer loop is O(n) and the inner two-pointer is O(n), giving O(n^2) overall.", generateStub, generateRunner
    });

    waves.push_back({
        305, "Partition the Kill", WORLD_NAME, WORLD_DESC,
        "Dutch national flag: rearrange so elements < target are left, == target are middle, > target are right. Return count of elements equal to target.",
        "Example: [3,1,2,3,3,4,5], target=3 -> 3 (three 3s in middle)\nExample: [1,2,3], target=5 -> 0\nExample: [5,5,5], target=5 -> 3",
        "1 <= N <= 500,000\nCorridor is NOT necessarily sorted.",
        500000, 1000, 300,
        "int closeJaws(vector<int>& corridor, int target)",
        "Dutch national flag algorithm using three pointers (lo, mid, hi). Elements less than target swap to lo, elements equal to target stay at mid, elements greater than target swap to hi. A single O(n) pass partitions everything into three regions.", generateStub, generateRunner
    });

    waves.push_back({
        306, "Merge the Packs", WORLD_NAME, WORLD_DESC,
        "Two sorted corridors. corridor has m elements followed by enough zero-padding for corridor2's n elements. Merge corridor2 into corridor in-place so the result is sorted. target = m (number of real elements in corridor). Return 0.",
        "Example: corridor=[1,3,5,0,0,0], corridor2=[2,4,6], target=3 -> 0 (corridor becomes [1,2,3,4,5,6])\nExample: corridor=[4,5,0], corridor2=[1], target=2 -> 0 (corridor becomes [1,4,5])",
        "0 <= m, n <= 200,000\nBoth sorted ascending.",
        200000, 1000, 300,
        "int closeJaws(vector<int>& corridor, vector<int>& corridor2, int target)",
        "Start from the ends of both real (sorted) sections and write backwards into the padded space. Compare the largest remaining element from each array, place the bigger one at the current write position, and decrement. O(n+m) time, O(1) extra space.", generateStub, generateRunner
    });

    waves.push_back({
        307, "The Pincer", WORLD_NAME, WORLD_DESC,
        "Two sorted corridors. Find one element from each that sum to target. Return 1 if such a pair exists, 0 otherwise. One pointer walks corridor forward, the other walks corridor2 backward.",
        "Example: [1,3,5], [2,4,6], target=9 -> 1 (3+6)\nExample: [1,2,3], [4,5,6], target=2 -> 0\nExample: [-1,0,5], [1,2,3], target=4 -> 1 (1+3... wait, no: -1+5=4? yes from same array. No — one from each: 1+3=4 yes)",
        "1 <= N, M <= 500,000\nBoth sorted ascending.",
        500000, 500, 300,
        "int closeJaws(vector<int>& corridor, vector<int>& corridor2, int target)",
        "Place one pointer at the start of corridor and one at the end of corridor2 (both sorted). If the sum is less than target, advance the left pointer. If greater, retreat the right pointer. O(n+m) time, O(1) space. The key insight is that sorted order lets you systematically narrow the search.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a two-pointer approach",
        "Need coordinated pointer movement across a sequence to avoid extra passes or storage",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Quickselect partitioning around a pivot",
            "Breadth-first traversal",
            "Dynamic programming table",
            "Trie for order statistics"
        },
        0,
        "Need global ordering or pivot-based partitioning to make progress efficiently",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given a sorted array `fangs` and an integer `target`, return `1` if there exists a pair of values whose sum is exactly `target`, otherwise return `0`.";
    waves[0].flavor_text = "A marsh trapper kneels at the bank and watches ripples pass over the hidden jaws below. Two weighted stones dropped at the right distances will snap the ancient trap shut; any other pairing leaves it hungry.";
    waves[1].clear_prompt = "Given a sorted array `fangs` and an integer `target`, return how many distinct pairs of values sum exactly to `target`.";
    waves[1].flavor_text = "The trapper has learned that one trigger is never enough. He counts every valid pair hidden in the ordered line of teeth, each one a different way to make the jaws close at the same moment.";
    waves[2].clear_prompt = "Given a sorted array `fangs`, remove duplicates in place and return the new logical length.";
    waves[2].flavor_text = "The trapper works a bone file between rows of white teeth, stripping away repeated carvings until only the true pattern remains. The line must stay ordered, but the echoes have to go.";
    waves[3].clear_prompt = "Given an array `fangs`, return all unique triplets of values whose sum is `0`.";
    waves[3].flavor_text = "He studies a spread of cracked ivory markers, searching for trios whose balance is perfect. Three points, one exact equilibrium, and the ancient mechanism beneath the marsh will stir.";
    waves[4].clear_prompt = "Given an array `fangs` and an integer `target`, reorder the array in place so values less than `target` come first, values equal to `target` come next, and values greater than `target` come last.";
    waves[4].flavor_text = "The jaws are fed by three channels, and the trapper must sort the offerings quickly: weaker teeth to the left gutter, matching teeth to the center, stronger teeth to the right. The order inside a gutter does not matter, only the partition.";
    waves[5].clear_prompt = "Given two sorted arrays, merge them into a single sorted array or sorted destination structure as specified by the wave.";
    waves[5].flavor_text = "Two rivers of bone splinters slide toward one another in the dark. The trapper works the seam where they meet, stitching them into one ordered current without losing a single shard.";
    waves[6].clear_prompt = "Given an unsorted array `fangs` and zero-indexed `target = k`, return the `k`th smallest value without fully sorting the array.";
    waves[6].flavor_text = "The trapper needs one exact tooth from a pile, not the luxury of arranging the entire heap into order. Somewhere in the mess is the rank he needs, and every discarded partition brings that answer closer.";
    return waves;
}

void closing_jaws::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    if (wave.id <= 305) {
        out << "int closeJaws(vector<int>& corridor, int target);\n\n";
    } else {
        out << "int closeJaws(vector<int>& corridor, vector<int>& corridor2, int target);\n\n";
    }

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 301: // Find the Pair
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 6};\n";
            out << "        if (closeJaws(c, 6) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3};\n";
            out << "        if (closeJaws(c, 7) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 5};\n";
            out << "        if (closeJaws(c, 6) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-3, -1, 0, 2, 4, 6};\n";
            out << "        if (closeJaws(c, 3) != 1) all_correct = false; // -1+4\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 1, 1};\n";
            out << "        if (closeJaws(c, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i;\n";
            out << "        // 0 + (n-1) = n-1\n";
            out << "        if (closeJaws(c, n - 1) != 1) all_correct = false;\n";
            out << "        // No pair sums to 2*n\n";
            out << "        if (closeJaws(c, 2 * n) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 302: // Count the Pairs
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (closeJaws(c, 6) != 2) all_correct = false; // 1+5, 2+4\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 1, 1};\n";
            out << "        if (closeJaws(c, 2) != 1) all_correct = false; // only one unique pair: 1+1\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3};\n";
            out << "        if (closeJaws(c, 10) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-2, -1, 0, 1, 2, 3};\n";
            out << "        if (closeJaws(c, 1) != 3) all_correct = false; // -2+3, -1+2, 0+1\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 1, 2, 2, 3, 3};\n";
            out << "        if (closeJaws(c, 4) != 2) all_correct = false; // 1+3, 2+2\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i;\n";
            out << "        // Pairs summing to n-1: (0,n-1), (1,n-2), ...\n";
            out << "        int expected = (n - 1) / 2;\n";
            out << "        if (closeJaws(c, n - 1) != expected) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 303: // The Squeeze
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (closeJaws(c, 8) != 8) all_correct = false; // 3+5\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (closeJaws(c, 100) != 9) all_correct = false; // 4+5\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-1, 2, 4, 8};\n";
            out << "        if (closeJaws(c, 5) != 6) all_correct = false; // 2+4\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 3};\n";
            out << "        if (closeJaws(c, 5) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-5, -3, -1, 2, 4};\n";
            out << "        if (closeJaws(c, 0) != -1) all_correct = false; // -1+2=1 or -3+2=-1... -1+2=1, -3+4=1, -5+4=-1. Closest to 0: 1 or -1 (both dist 1)\n";
            out << "        int r = closeJaws(c, 0);\n";
            out << "        if (r != 1 && r != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i * 2; // 0,2,4,...,2(n-1)\n";
            out << "        // target = n: closest pair sum to n\n";
            out << "        // e.g. n=200000, pairs: ... many near n\n";
            out << "        int r = closeJaws(c, n);\n";
            out << "        if (r != n) all_correct = false; // n/2*2 + (n/2-1)*2 = n + n - 2? No. 0+n=n if n is even.\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 304: // Triple Hunt
            out << "    {\n";
            out << "        vector<int> c = {-4, -1, -1, 0, 1, 2};\n";
            out << "        if (closeJaws(c, 0) != 1) all_correct = false; // -1+-1+2=0\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3};\n";
            out << "        if (closeJaws(c, 7) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {0, 0, 0};\n";
            out << "        if (closeJaws(c, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (closeJaws(c, 12) != 1) all_correct = false; // 3+4+5\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3, 4, 5};\n";
            out << "        if (closeJaws(c, 6) != 1) all_correct = false; // 1+2+3\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-10, -5, 0, 5, 10};\n";
            out << "        if (closeJaws(c, 0) != 1) all_correct = false; // -10+0+10\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i;\n";
            out << "        // 0+1+(n-1) = n -> should find it\n";
            out << "        if (closeJaws(c, n) != 1) all_correct = false;\n";
            out << "        // 3*n can't be reached (max triple = (n-1)+(n-2)+(n-3))\n";
            out << "        if (closeJaws(c, 3 * n) != 0) all_correct = false;\n";
            out << "        ops = n * n;\n";
            out << "    } else { ops = 22; }\n";
            break;

        case 305: // Partition the Kill (Dutch National Flag)
            out << "    {\n";
            out << "        vector<int> c = {3, 1, 2, 3, 3, 4, 5};\n";
            out << "        int cnt = closeJaws(c, 3);\n";
            out << "        if (cnt != 3) all_correct = false;\n";
            out << "        // Verify partition: all < 3, then all == 3, then all > 3\n";
            out << "        int i = 0;\n";
            out << "        while (i < (int)c.size() && c[i] < 3) i++;\n";
            out << "        int mid_start = i;\n";
            out << "        while (i < (int)c.size() && c[i] == 3) i++;\n";
            out << "        if (i - mid_start != 3) all_correct = false;\n";
            out << "        while (i < (int)c.size()) { if (c[i] <= 3) all_correct = false; i++; }\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3};\n";
            out << "        if (closeJaws(c, 5) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {5, 5, 5};\n";
            out << "        if (closeJaws(c, 5) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {2, 0, 2, 1, 1, 0};\n";
            out << "        int cnt = closeJaws(c, 1);\n";
            out << "        if (cnt != 2) all_correct = false;\n";
            out << "        // Check: 0s then 1s then 2s\n";
            out << "        int j = 0;\n";
            out << "        while (j < (int)c.size() && c[j] < 1) j++;\n";
            out << "        while (j < (int)c.size() && c[j] == 1) j++;\n";
            out << "        while (j < (int)c.size()) { if (c[j] <= 1) all_correct = false; j++; }\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n);\n";
            out << "        for (int i = 0; i < n; i++) c[i] = i % 3; // 0,1,2,0,1,2,...\n";
            out << "        int cnt = closeJaws(c, 1);\n";
            out << "        int expected_ones = (n + 1) / 3;\n";
            out << "        if (cnt != expected_ones) all_correct = false;\n";
            out << "        // Verify partition\n";
            out << "        int j = 0;\n";
            out << "        while (j < n && c[j] < 1) j++;\n";
            out << "        while (j < n && c[j] == 1) j++;\n";
            out << "        while (j < n) { if (c[j] <= 1) { all_correct = false; break; } j++; }\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 22; }\n";
            break;

        case 306: // Merge the Packs
            out << "    {\n";
            out << "        vector<int> c = {1, 3, 5, 0, 0, 0}, c2 = {2, 4, 6};\n";
            out << "        closeJaws(c, c2, 3);\n";
            out << "        vector<int> expected = {1, 2, 3, 4, 5, 6};\n";
            out << "        if (c != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {4, 5, 0}, c2 = {1};\n";
            out << "        closeJaws(c, c2, 2);\n";
            out << "        vector<int> expected = {1, 4, 5};\n";
            out << "        if (c != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {0, 0, 0}, c2 = {1, 2, 3};\n";
            out << "        closeJaws(c, c2, 0);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (c != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3}, c2 = {};\n";
            out << "        closeJaws(c, c2, 3);\n";
            out << "        vector<int> expected = {1, 2, 3};\n";
            out << "        if (c != expected) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        int m = n / 2;\n";
            out << "        vector<int> c(n, 0), c2(n - m);\n";
            out << "        for (int i = 0; i < m; i++) c[i] = i * 2;       // 0,2,4,...\n";
            out << "        for (int i = 0; i < n - m; i++) c2[i] = i * 2 + 1; // 1,3,5,...\n";
            out << "        closeJaws(c, c2, m);\n";
            out << "        for (int i = 0; i < n; i++) if (c[i] != i) { all_correct = false; break; }\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 307: // The Pincer (boss)
            out << "    {\n";
            out << "        vector<int> c = {1, 3, 5}, c2 = {2, 4, 6};\n";
            out << "        if (closeJaws(c, c2, 9) != 1) all_correct = false; // 3+6\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1, 2, 3}, c2 = {4, 5, 6};\n";
            out << "        if (closeJaws(c, c2, 2) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-5, -3, -1}, c2 = {1, 3, 5};\n";
            out << "        if (closeJaws(c, c2, 0) != 1) all_correct = false; // -5+5, -3+3, -1+1\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1}, c2 = {1};\n";
            out << "        if (closeJaws(c, c2, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {1}, c2 = {1};\n";
            out << "        if (closeJaws(c, c2, 3) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> c = {-10, 0, 10, 20}, c2 = {-20, -10, 0, 10};\n";
            out << "        if (closeJaws(c, c2, 10) != 1) all_correct = false; // 0+10, 10+0, 20+(-10)\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> c(n), c2(n);\n";
            out << "        for (int i = 0; i < n; i++) { c[i] = i; c2[i] = i; }\n";
            out << "        // c[0]+c2[n-1] = 0+(n-1) = n-1\n";
            out << "        if (closeJaws(c, c2, n - 1) != 1) all_correct = false;\n";
            out << "        // No pair can sum to 2*n (max is (n-1)+(n-1)=2n-2)\n";
            out << "        if (closeJaws(c, c2, 2 * n) != 0) all_correct = false;\n";
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

    out << "const { closeJaws } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 301: // Find the Pair
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 6];\n";
            out << "    if (closeJaws(c, 6) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3];\n";
            out << "    if (closeJaws(c, 7) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 5];\n";
            out << "    if (closeJaws(c, 6) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-3, -1, 0, 2, 4, 6];\n";
            out << "    if (closeJaws(c, 3) !== 1) allCorrect = false; // -1+4\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 1, 1, 1];\n";
            out << "    if (closeJaws(c, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i;\n";
            out << "    // 0 + (n-1) = n-1\n";
            out << "    if (closeJaws(c, n - 1) !== 1) allCorrect = false;\n";
            out << "    // No pair sums to 2*n\n";
            out << "    if (closeJaws(c, 2 * n) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 302: // Count the Pairs
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 5];\n";
            out << "    if (closeJaws(c, 6) !== 2) allCorrect = false; // 1+5, 2+4\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 1, 1, 1];\n";
            out << "    if (closeJaws(c, 2) !== 1) allCorrect = false; // only one unique pair: 1+1\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3];\n";
            out << "    if (closeJaws(c, 10) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-2, -1, 0, 1, 2, 3];\n";
            out << "    if (closeJaws(c, 1) !== 3) allCorrect = false; // -2+3, -1+2, 0+1\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 1, 2, 2, 3, 3];\n";
            out << "    if (closeJaws(c, 4) !== 2) allCorrect = false; // 1+3, 2+2\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i;\n";
            out << "    // Pairs summing to n-1: (0,n-1), (1,n-2), ...\n";
            out << "    let expected = Math.floor((n - 1) / 2);\n";
            out << "    if (closeJaws(c, n - 1) !== expected) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 303: // The Squeeze
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 5];\n";
            out << "    if (closeJaws(c, 8) !== 8) allCorrect = false; // 3+5\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 5];\n";
            out << "    if (closeJaws(c, 100) !== 9) allCorrect = false; // 4+5\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-1, 2, 4, 8];\n";
            out << "    if (closeJaws(c, 5) !== 6) allCorrect = false; // 2+4\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 3];\n";
            out << "    if (closeJaws(c, 5) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-5, -3, -1, 2, 4];\n";
            out << "    let r = closeJaws(c, 0);\n";
            out << "    if (r !== 1 && r !== -1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i * 2; // 0,2,4,...,2(n-1)\n";
            out << "    // target = n: closest pair sum to n\n";
            out << "    let r = closeJaws(c, n);\n";
            out << "    if (r !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 304: // Triple Hunt
            out << "{\n";
            out << "    let c = [-4, -1, -1, 0, 1, 2];\n";
            out << "    if (closeJaws(c, 0) !== 1) allCorrect = false; // -1+-1+2=0\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3];\n";
            out << "    if (closeJaws(c, 7) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [0, 0, 0];\n";
            out << "    if (closeJaws(c, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 5];\n";
            out << "    if (closeJaws(c, 12) !== 1) allCorrect = false; // 3+4+5\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3, 4, 5];\n";
            out << "    if (closeJaws(c, 6) !== 1) allCorrect = false; // 1+2+3\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-10, -5, 0, 5, 10];\n";
            out << "    if (closeJaws(c, 0) !== 1) allCorrect = false; // -10+0+10\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i;\n";
            out << "    // 0+1+(n-1) = n -> should find it\n";
            out << "    if (closeJaws(c, n) !== 1) allCorrect = false;\n";
            out << "    // 3*n can't be reached (max triple = (n-1)+(n-2)+(n-3))\n";
            out << "    if (closeJaws(c, 3 * n) !== 0) allCorrect = false;\n";
            out << "    ops = n * n;\n";
            out << "} else { ops = 22; }\n";
            break;

        case 305: // Partition the Kill (Dutch National Flag)
            out << "{\n";
            out << "    let c = [3, 1, 2, 3, 3, 4, 5];\n";
            out << "    let cnt = closeJaws(c, 3);\n";
            out << "    if (cnt !== 3) allCorrect = false;\n";
            out << "    // Verify partition: all < 3, then all == 3, then all > 3\n";
            out << "    let i = 0;\n";
            out << "    while (i < c.length && c[i] < 3) i++;\n";
            out << "    let midStart = i;\n";
            out << "    while (i < c.length && c[i] === 3) i++;\n";
            out << "    if (i - midStart !== 3) allCorrect = false;\n";
            out << "    while (i < c.length) { if (c[i] <= 3) allCorrect = false; i++; }\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3];\n";
            out << "    if (closeJaws(c, 5) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [5, 5, 5];\n";
            out << "    if (closeJaws(c, 5) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [2, 0, 2, 1, 1, 0];\n";
            out << "    let cnt = closeJaws(c, 1);\n";
            out << "    if (cnt !== 2) allCorrect = false;\n";
            out << "    // Check: 0s then 1s then 2s\n";
            out << "    let j = 0;\n";
            out << "    while (j < c.length && c[j] < 1) j++;\n";
            out << "    while (j < c.length && c[j] === 1) j++;\n";
            out << "    while (j < c.length) { if (c[j] <= 1) allCorrect = false; j++; }\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) c[i] = i % 3; // 0,1,2,0,1,2,...\n";
            out << "    let cnt = closeJaws(c, 1);\n";
            out << "    let expectedOnes = Math.floor((n + 1) / 3);\n";
            out << "    if (cnt !== expectedOnes) allCorrect = false;\n";
            out << "    // Verify partition\n";
            out << "    let j = 0;\n";
            out << "    while (j < n && c[j] < 1) j++;\n";
            out << "    while (j < n && c[j] === 1) j++;\n";
            out << "    while (j < n) { if (c[j] <= 1) { allCorrect = false; break; } j++; }\n";
            out << "    ops = n;\n";
            out << "} else { ops = 22; }\n";
            break;

        case 306: // Merge the Packs
            out << "{\n";
            out << "    let c = [1, 3, 5, 0, 0, 0], c2 = [2, 4, 6];\n";
            out << "    closeJaws(c, c2, 3);\n";
            out << "    let expected = [1, 2, 3, 4, 5, 6];\n";
            out << "    if (JSON.stringify(c) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [4, 5, 0], c2 = [1];\n";
            out << "    closeJaws(c, c2, 2);\n";
            out << "    let expected = [1, 4, 5];\n";
            out << "    if (JSON.stringify(c) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [0, 0, 0], c2 = [1, 2, 3];\n";
            out << "    closeJaws(c, c2, 0);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(c) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3], c2 = [];\n";
            out << "    closeJaws(c, c2, 3);\n";
            out << "    let expected = [1, 2, 3];\n";
            out << "    if (JSON.stringify(c) !== JSON.stringify(expected)) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let m = Math.floor(n / 2);\n";
            out << "    let c = new Array(n).fill(0), c2 = new Array(n - m);\n";
            out << "    for (let i = 0; i < m; i++) c[i] = i * 2;       // 0,2,4,...\n";
            out << "    for (let i = 0; i < n - m; i++) c2[i] = i * 2 + 1; // 1,3,5,...\n";
            out << "    closeJaws(c, c2, m);\n";
            out << "    for (let i = 0; i < n; i++) if (c[i] !== i) { allCorrect = false; break; }\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 307: // The Pincer (boss)
            out << "{\n";
            out << "    let c = [1, 3, 5], c2 = [2, 4, 6];\n";
            out << "    if (closeJaws(c, c2, 9) !== 1) allCorrect = false; // 3+6\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1, 2, 3], c2 = [4, 5, 6];\n";
            out << "    if (closeJaws(c, c2, 2) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-5, -3, -1], c2 = [1, 3, 5];\n";
            out << "    if (closeJaws(c, c2, 0) !== 1) allCorrect = false; // -5+5, -3+3, -1+1\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1], c2 = [1];\n";
            out << "    if (closeJaws(c, c2, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [1], c2 = [1];\n";
            out << "    if (closeJaws(c, c2, 3) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let c = [-10, 0, 10, 20], c2 = [-20, -10, 0, 10];\n";
            out << "    if (closeJaws(c, c2, 10) !== 1) allCorrect = false; // 0+10, 10+0, 20+(-10)\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let c = new Array(n), c2 = new Array(n);\n";
            out << "    for (let i = 0; i < n; i++) { c[i] = i; c2[i] = i; }\n";
            out << "    // c[0]+c2[n-1] = 0+(n-1) = n-1\n";
            out << "    if (closeJaws(c, c2, n - 1) !== 1) allCorrect = false;\n";
            out << "    // No pair can sum to 2*n (max is (n-1)+(n-1)=2n-2)\n";
            out << "    if (closeJaws(c, c2, 2 * n) !== 0) allCorrect = false;\n";
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
