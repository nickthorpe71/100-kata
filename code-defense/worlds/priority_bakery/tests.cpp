#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> priority_bakery::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1201, "Kth Largest", WORLD_NAME, WORLD_DESC,
        "Find the kth largest element in orders. target is k (1-indexed). Return that element.",
        "Example: [3,2,1,5,6,4] k=2 -> 5\nExample: [3,2,3,1,2,4,5,5,6] k=4 -> 4\nExample: [1] k=1 -> 1",
        "1 <= N <= 500,000\n1 <= k <= N",
        500000, 1000, 300,
        "int bakePriority(vector<int>& orders, int target)",
        "Use a min-heap of size k. Process each element: if heap size < k, push. If element > heap top, pop and push. Final heap top is kth largest. O(n log k).", generateStub, generateRunner
    });

    waves.push_back({
        1202, "Sort the Queue", WORLD_NAME, WORLD_DESC,
        "Heap sort orders in ascending order (in-place). Return 0. After the call, orders must be sorted ascending.",
        "Example: [3,1,4,1,5] -> sorted to [1,1,3,4,5], return 0\nExample: [5,4,3,2,1] -> sorted to [1,2,3,4,5], return 0\nExample: [1] -> [1], return 0",
        "1 <= N <= 500,000",
        500000, 1000, 300,
        "int bakePriority(vector<int>& orders, int target)",
        "Build a max-heap from the array (heapify). Repeatedly extract max to the end of the array. O(n log n).", generateStub, generateRunner
    });

    waves.push_back({
        1203, "Top K Sum", WORLD_NAME, WORLD_DESC,
        "Return the sum of the target largest elements in orders. target is k.",
        "Example: [3,1,4,1,5,9] k=3 -> 18 (9+5+4)\nExample: [1,1,1] k=2 -> 2\nExample: [5] k=1 -> 5",
        "1 <= N <= 500,000\n1 <= k <= N",
        500000, 1000, 300,
        "int bakePriority(vector<int>& orders, int target)",
        "Use a min-heap of size k (same as wave 1). Sum all elements remaining in the heap. O(n log k).", generateStub, generateRunner
    });

    waves.push_back({
        1204, "Merge the Kitchens", WORLD_NAME, WORLD_DESC,
        "orders contains k sorted subsequences separated by -1. target is k. Merge them into a single sorted sequence (store result back in orders). Return the total element count (excluding -1 separators).",
        "Example: [1,3,5,-1,2,4,6] k=2 -> 6\nExample: [1,2,3] k=1 -> 3\nExample: [1,-1,2,-1,3] k=3 -> 3",
        "1 <= total elements <= 100,000\n1 <= k <= 100",
        100000, 1000, 300,
        "int bakePriority(vector<int>& orders, int target)",
        "Min-heap of (value, subsequence_index, position). Pop smallest, push next from that subsequence. Collect results. O(total * log k).", generateStub, generateRunner
    });

    waves.push_back({
        1205, "Running Median (boss)", WORLD_NAME, WORLD_DESC,
        "Process orders left to right, maintaining a running median. Return floor(final_median * 100). target is unused.",
        "Example: [1,2,3] -> 200 (median=2.0)\nExample: [2,1] -> 150 (median=1.5)\nExample: [5] -> 500\nExample: [1,5,3] -> 300 (median=3.0)",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int bakePriority(vector<int>& orders, int target)",
        "Two heaps -- max-heap for lower half, min-heap for upper half. Balance sizes after each insertion. Median is top of max-heap (odd count) or average of both tops (even). O(n log n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a heap-based approach",
        "Need repeated access to the current smallest or largest frontier element",
        "O(k) space"
    );
    waves[1].quiz = quizbank::makeThemedQuiz(
        "a heap-based approach",
        "Need repeated access to the current smallest or largest frontier element",
        "O(n log n) time / O(1) space"
    );
    waves[4].quiz = quizbank::makeThemedQuiz(
        "a heap-based approach",
        "Need repeated access to the current smallest or largest frontier element",
        "O(n log n) time / O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Two heaps balancing lower and upper halves",
            "Sliding window median over the array",
            "Binary search on the final median value",
            "Union-Find over close values"
        },
        0,
        "Need repeated access to the current smallest or largest frontier element",
        "O(n log n) time / O(n) space"
    );
    waves[0].clear_prompt = "Given an array `orders` and integer `target = k`, return the `k`th largest element.";
    waves[0].flavor_text = "In the Priority Bakery, the head expediter watches trays of orders stack up faster than bread can cool. He keeps only the most important requests in reach while the rest pass under his hands.";
    waves[1].clear_prompt = "Given an array `orders`, sort it in ascending order using heap sort and return `0`.";
    waves[1].flavor_text = "The bakery floor shakes with conveyor belts and iron trays. The expediter builds a heap from the chaos, then draws the largest loaf into its final place again and again.";
    waves[2].clear_prompt = "Given an array `orders` and integer `target = k`, return the sum of the `k` largest elements.";
    waves[2].flavor_text = "Sometimes the bakers care less about one champion loaf than the combined weight of the top shelves. The expediter keeps the heaviest few in mind and lets the crumbs fall away.";
    waves[3].clear_prompt = "Given `orders` encoding `k` sorted subsequences separated as described by the wave, merge them into one sorted sequence and return the number of merged elements.";
    waves[3].flavor_text = "Multiple kitchen lines are already sorted, but service needs one unbroken queue. The expediter stands at the junction, always choosing the smallest ready item from the front of every stream.";
    waves[4].clear_prompt = "Given an array `orders`, process it left to right and return the final running median as encoded by the wave.";
    waves[4].flavor_text = "The bakery's orders arrive one by one, and the expediter must keep the center of the demand curve balanced at every moment. Lean too far left or right and the whole kitchen loses its rhythm.";
    return waves;
}

void priority_bakery::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/runner.cpp";
        std::ofstream out(path);

        out << "#include <vector>\n";
        out << "#include <string>\n";
        out << "#include <chrono>\n";
        out << "#include <cstdio>\n";
        out << "#include <cstdlib>\n";
        out << "#include <algorithm>\n";
        out << "#include <queue>\n";
        out << "#include <functional>\n";
        out << "using namespace std;\n\n";

        out << "int bakePriority(vector<int>& orders, int target);\n\n";

        out << "int main(int argc, char* argv[]) {\n";
        out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
        out << "    bool all_correct = true;\n";
        out << "    int ops = 0;\n\n";
        out << "    auto start = chrono::high_resolution_clock::now();\n\n";

        switch (wave.id) {
            case 1201:
                out << "    {\n";
                out << "        vector<int> o = {3, 2, 1, 5, 6, 4};\n";
                out << "        if (bakePriority(o, 2) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {3, 2, 3, 1, 2, 4, 5, 5, 6};\n";
                out << "        if (bakePriority(o, 4) != 4) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1};\n";
                out << "        if (bakePriority(o, 1) != 1) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> o(n);\n";
                out << "        for (int i = 0; i < n; i++) o[i] = i + 1;\n";
                out << "        if (bakePriority(o, 1) != n) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 16; }\n";
                break;

            case 1202:
                out << "    {\n";
                out << "        vector<int> o = {3, 1, 4, 1, 5};\n";
                out << "        bakePriority(o, 0);\n";
                out << "        vector<int> expected = {1, 1, 3, 4, 5};\n";
                out << "        if (o != expected) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {5, 4, 3, 2, 1};\n";
                out << "        bakePriority(o, 0);\n";
                out << "        vector<int> expected = {1, 2, 3, 4, 5};\n";
                out << "        if (o != expected) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1};\n";
                out << "        bakePriority(o, 0);\n";
                out << "        vector<int> expected = {1};\n";
                out << "        if (o != expected) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> o(n);\n";
                out << "        for (int i = 0; i < n; i++) o[i] = n - i;\n";
                out << "        bakePriority(o, 0);\n";
                out << "        bool sorted = true;\n";
                out << "        for (int i = 1; i < n; i++) if (o[i] < o[i-1]) { sorted = false; break; }\n";
                out << "        if (!sorted) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 11; }\n";
                break;

            case 1203:
                out << "    {\n";
                out << "        vector<int> o = {3, 1, 4, 1, 5, 9};\n";
                out << "        if (bakePriority(o, 3) != 18) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1, 1, 1};\n";
                out << "        if (bakePriority(o, 2) != 2) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {5};\n";
                out << "        if (bakePriority(o, 1) != 5) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> o(n, 1);\n";
                out << "        if (bakePriority(o, 100) != 100) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 10; }\n";
                break;

            case 1204:
                out << "    {\n";
                out << "        vector<int> o = {1, 3, 5, -1, 2, 4, 6};\n";
                out << "        if (bakePriority(o, 2) != 6) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1, 2, 3};\n";
                out << "        if (bakePriority(o, 1) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1, -1, 2, -1, 3};\n";
                out << "        if (bakePriority(o, 3) != 3) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int k = 100, sz = 1000;\n";
                out << "        vector<int> o;\n";
                out << "        for (int i = 0; i < k; i++) {\n";
                out << "            if (i > 0) o.push_back(-1);\n";
                out << "            for (int j = 0; j < sz; j++) o.push_back(i * sz + j);\n";
                out << "        }\n";
                out << "        if (bakePriority(o, k) != k * sz) all_correct = false;\n";
                out << "        ops = k * sz;\n";
                out << "    } else { ops = 13; }\n";
                break;

            case 1205:
                out << "    {\n";
                out << "        vector<int> o = {1, 2, 3};\n";
                out << "        if (bakePriority(o, 0) != 200) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {2, 1};\n";
                out << "        if (bakePriority(o, 0) != 150) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {5};\n";
                out << "        if (bakePriority(o, 0) != 500) all_correct = false;\n";
                out << "    }\n";
                out << "    {\n";
                out << "        vector<int> o = {1, 5, 3};\n";
                out << "        if (bakePriority(o, 0) != 300) all_correct = false;\n";
                out << "    }\n";
                out << "    if (!test_only) {\n";
                out << "        int n = " << wave.n << ";\n";
                out << "        vector<int> o(n);\n";
                out << "        for (int i = 0; i < n; i++) o[i] = i + 1;\n";
                out << "        int result = bakePriority(o, 0);\n";
                out << "        int expected = (int)((double)(n + 1) / 2.0 * 100.0);\n";
                out << "        if (result != expected) all_correct = false;\n";
                out << "        ops = n;\n";
                out << "    } else { ops = 11; }\n";
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

        out << "const { bakePriority } = require('./solution');\n";
        out << "const testOnly = process.argv.includes('--test');\n";
        out << "let allCorrect = true;\n";
        out << "let ops = 0;\n";
        out << "const start = process.hrtime.bigint();\n\n";

        switch (wave.id) {
            case 1201:
                out << "if (bakePriority([3, 2, 1, 5, 6, 4], 2) !== 5) allCorrect = false;\n";
                out << "if (bakePriority([3, 2, 3, 1, 2, 4, 5, 5, 6], 4) !== 4) allCorrect = false;\n";
                out << "if (bakePriority([1], 1) !== 1) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const o = [];\n";
                out << "    for (let i = 0; i < n; i++) o.push(i + 1);\n";
                out << "    if (bakePriority(o, 1) !== n) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 16; }\n";
                break;

            case 1202:
                out << "{\n";
                out << "    const o = [3, 1, 4, 1, 5];\n";
                out << "    bakePriority(o, 0);\n";
                out << "    if (JSON.stringify(o) !== JSON.stringify([1, 1, 3, 4, 5])) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    const o = [5, 4, 3, 2, 1];\n";
                out << "    bakePriority(o, 0);\n";
                out << "    if (JSON.stringify(o) !== JSON.stringify([1, 2, 3, 4, 5])) allCorrect = false;\n";
                out << "}\n";
                out << "{\n";
                out << "    const o = [1];\n";
                out << "    bakePriority(o, 0);\n";
                out << "    if (JSON.stringify(o) !== JSON.stringify([1])) allCorrect = false;\n";
                out << "}\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const o = [];\n";
                out << "    for (let i = 0; i < n; i++) o.push(n - i);\n";
                out << "    bakePriority(o, 0);\n";
                out << "    let sorted = true;\n";
                out << "    for (let i = 1; i < n; i++) if (o[i] < o[i-1]) { sorted = false; break; }\n";
                out << "    if (!sorted) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 11; }\n";
                break;

            case 1203:
                out << "if (bakePriority([3, 1, 4, 1, 5, 9], 3) !== 18) allCorrect = false;\n";
                out << "if (bakePriority([1, 1, 1], 2) !== 2) allCorrect = false;\n";
                out << "if (bakePriority([5], 1) !== 5) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const o = new Array(n).fill(1);\n";
                out << "    if (bakePriority(o, 100) !== 100) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 10; }\n";
                break;

            case 1204:
                out << "if (bakePriority([1, 3, 5, -1, 2, 4, 6], 2) !== 6) allCorrect = false;\n";
                out << "if (bakePriority([1, 2, 3], 1) !== 3) allCorrect = false;\n";
                out << "if (bakePriority([1, -1, 2, -1, 3], 3) !== 3) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const k = 100, sz = 1000;\n";
                out << "    const o = [];\n";
                out << "    for (let i = 0; i < k; i++) {\n";
                out << "        if (i > 0) o.push(-1);\n";
                out << "        for (let j = 0; j < sz; j++) o.push(i * sz + j);\n";
                out << "    }\n";
                out << "    if (bakePriority(o, k) !== k * sz) allCorrect = false;\n";
                out << "    ops = k * sz;\n";
                out << "} else { ops = 13; }\n";
                break;

            case 1205:
                out << "if (bakePriority([1, 2, 3], 0) !== 200) allCorrect = false;\n";
                out << "if (bakePriority([2, 1], 0) !== 150) allCorrect = false;\n";
                out << "if (bakePriority([5], 0) !== 500) allCorrect = false;\n";
                out << "if (bakePriority([1, 5, 3], 0) !== 300) allCorrect = false;\n";
                out << "if (!testOnly) {\n";
                out << "    const n = " << wave.n << ";\n";
                out << "    const o = [];\n";
                out << "    for (let i = 0; i < n; i++) o.push(i + 1);\n";
                out << "    const result = bakePriority(o, 0);\n";
                out << "    const expected = Math.floor((n + 1) / 2.0 * 100.0);\n";
                out << "    if (result !== expected) allCorrect = false;\n";
                out << "    ops = n;\n";
                out << "} else { ops = 11; }\n";
                break;
        }

        out << "\nconst end = process.hrtime.bigint();\n";
        out << "const ms = Number((end - start) / 1000000n);\n";
        out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
        out << "process.exit(allCorrect ? 0 : 1);\n";

        out.close();
    }
}
