#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> flow_lab::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2201, "Distinct Window", WORLD_NAME, WORLD_DESC,
        "Find the longest subarray with at most target distinct values. Return its length.",
        "Example: [1,2,1,2,3] target=2 -> 4 ([1,2,1,2])\nExample: [1,2,3,4] target=1 -> 1\nExample: [1,1,1] target=1 -> 3",
        "1 <= N <= 500,000\n1 <= target <= N",
        500000, 1000, 300,
        "int analyzeFlow(vector<int>& stream, int target)",
        "Sliding window with hash map tracking value frequencies. Expand right, adding to map. When distinct count > target, shrink from left (decrement, remove if zero). Track max window size. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2202, "Full Spectrum", WORLD_NAME, WORLD_DESC,
        "Find the smallest subarray containing ALL distinct values present in the stream. Return its length. target is unused.",
        "Example: [1,2,1,3,2] -> 3 ([1,3,2] or [2,1,3])\nExample: [1,1,1] -> 1\nExample: [1,2,3,1,2,3] -> 3",
        "1 <= N <= 500,000\ntarget is unused.",
        500000, 1000, 300,
        "int analyzeFlow(vector<int>& stream, int target)",
        "First count total distinct values D. Then sliding window: expand right, when window contains all D values, try shrinking from left. Track minimum window length. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2203, "Exact Diversity", WORLD_NAME, WORLD_DESC,
        "Count the number of subarrays with exactly target distinct values. Use the at_most(k) - at_most(k-1) technique.",
        "Example: [1,2,1,2,3] target=2 -> 7\nExample: [1,2,1,3,4] target=3 -> 3\nExample: [1,1,1] target=1 -> 6",
        "1 <= N <= 100,000\n1 <= target <= N",
        100000, 2000, 300,
        "int analyzeFlow(vector<int>& stream, int target)",
        "Use the at-most-k trick: count(exactly k) = count(at most k) - count(at most k-1). Each at-most-k uses the sliding window from wave 1. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2204, "No Repeats", WORLD_NAME, WORLD_DESC,
        "Find the longest subarray with all unique elements (no repeats). Return its length. target is unused.",
        "Example: [1,2,3,1,2,3] -> 3\nExample: [1,1,1] -> 1\nExample: [1,2,3,4,5] -> 5\nExample: [2,1,3,2,1] -> 3",
        "1 <= N <= 500,000\n0 <= stream[i] <= 999\ntarget is unused.",
        500000, 1000, 300,
        "int analyzeFlow(vector<int>& stream, int target)",
        "Sliding window with hash map/set. Expand right. If duplicate found, shrink from left until the duplicate is removed. Track max window. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2205, "The Full Analysis", WORLD_NAME, WORLD_DESC,
        "Minimum window in stream that contains all elements from a target set. stream[0] = size of target set, stream[1..stream[0]] = target values, stream[stream[0]+1..] = the actual stream. Return min window length, or 0 if impossible.",
        "Example: [3,1,2,3,7,1,2,1,3,2,1] -> 3 (target={1,2,3}, stream=[7,1,2,1,3,2,1])\nExample: [2,1,2,1,1,1] -> 0 (target={1,2}, stream=[1,1,1], no 2)",
        "1 <= target set size <= 100\n1 <= stream length <= 500,000",
        500000, 2000, 300,
        "int analyzeFlow(vector<int>& stream, int target)",
        "Sliding window with hash map tracking how many target values are covered. Expand right until all target values present, then shrink left to minimize. Track minimum window. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a sliding-window plus hash-map approach",
        "Need to maintain a contiguous window while updating counts incrementally",
        "O(k) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Sliding window with frequency counts",
            "Union-Find over equal values",
            "Binary search on window size",
            "Heap of most frequent elements"
        },
        0,
        "Need to maintain a contiguous window while updating counts incrementally",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(k) space")
    );
    waves[0].clear_prompt = "Given an array and a window rule about distinct values, compute the required answer for contiguous windows while tracking value counts.";
    waves[0].flavor_text = "Inside the Flow Lab, an alchemist watches colored liquids race through glass channels under magnesium light. She studies one segment at a time, counting which reagents are present before the current slips away.";
    waves[1].clear_prompt = "Given an array or string and a required set of values, find the smallest contiguous window that contains the full required spectrum.";
    waves[1].flavor_text = "The alchemist watches colored reagents pool and scatter through transparent pipes. Somewhere in the stream is the first span that contains every ingredient at once, the exact mixture needed to keep the apparatus alive.";
    waves[2].clear_prompt = "Given an array and a target distinct-count condition, return the number or size of contiguous windows that satisfy it, as specified by the wave.";
    waves[2].flavor_text = "Every chamber in the laboratory hums with a different hue, and the alchemist must measure diversity with precision. Too few distinct colors and the reaction dies; too many and the glass begins to crack.";
    waves[3].clear_prompt = "Given a string or array, return the length of the longest contiguous window with no repeated values.";
    waves[3].flavor_text = "The alchemist follows a beam of shifting symbols through the lab and searches for the longest flawless run, a sequence unbroken by any repeated mark.";
    waves[4].clear_prompt = "Given an array or string and a full-coverage requirement, return the best contiguous window according to the wave's exact rule.";
    waves[4].flavor_text = "At the center of the Flow Lab, the channels converge into one dangerous reading. The alchemist must keep the live sample as tight as possible while still preserving every signal that matters.";
    return waves;
}

void flow_lab::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <vector>\n";
    out << "#include <string>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <unordered_map>\n";
    out << "#include <unordered_set>\n";
    out << "#include <algorithm>\n";
    out << "using namespace std;\n\n";

    out << "int analyzeFlow(vector<int>& stream, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 2201:
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 2) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 4};\n";
            out << "        if (analyzeFlow(s, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 1, 1};\n";
            out << "        if (analyzeFlow(s, 1) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 1, 3, 4};\n";
            out << "        if (analyzeFlow(s, 3) != 4) all_correct = false; // [1,2,1,3] or [2,1,3,4]\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1};\n";
            out << "        if (analyzeFlow(s, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = i % 100;\n";
            out << "        if (analyzeFlow(s, 100) != n) all_correct = false;\n";
            out << "        if (analyzeFlow(s, 1) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 2202:
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 1, 3, 2};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 1, 1};\n";
            out << "        if (analyzeFlow(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 4, 5};\n";
            out << "        if (analyzeFlow(s, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {7};\n";
            out << "        if (analyzeFlow(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = i % 50;\n";
            out << "        if (analyzeFlow(s, 0) != 50) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 2203:
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 2) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 1, 3, 4};\n";
            out << "        if (analyzeFlow(s, 3) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 1, 1};\n";
            out << "        if (analyzeFlow(s, 1) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 1) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 3) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = i % 10;\n";
            out << "        if (analyzeFlow(s, 1) != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 2204:
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 1, 1};\n";
            out << "        if (analyzeFlow(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 4, 5};\n";
            out << "        if (analyzeFlow(s, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {2, 1, 3, 2, 1};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {5};\n";
            out << "        if (analyzeFlow(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 2, 3, 4, 2, 5, 6};\n";
            out << "        if (analyzeFlow(s, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s(n);\n";
            out << "        for (int i = 0; i < n; i++) s[i] = i % 1000;\n";
            out << "        if (analyzeFlow(s, 0) != 1000) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 21; }\n";
            break;

        case 2205:
            out << "    {\n";
            out << "        vector<int> s = {3, 1, 2, 3, 7, 1, 2, 1, 3, 2, 1};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {2, 1, 2, 1, 1, 1};\n";
            out << "        if (analyzeFlow(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 5, 1, 2, 5, 3};\n";
            out << "        if (analyzeFlow(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {3, 1, 2, 3, 1, 2, 3};\n";
            out << "        if (analyzeFlow(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {2, 1, 2, 1, 1, 1, 2};\n";
            out << "        if (analyzeFlow(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int tgt_size = 100;\n";
            out << "        int stream_len = " << wave.n << ";\n";
            out << "        vector<int> s;\n";
            out << "        s.push_back(tgt_size);\n";
            out << "        for (int i = 1; i <= tgt_size; i++) s.push_back(i);\n";
            out << "        for (int i = 0; i < stream_len; i++) s.push_back((i % 200) + 1);\n";
            out << "        int r = analyzeFlow(s, 0);\n";
            out << "        if (r != 100) all_correct = false;\n";
            out << "        ops = stream_len;\n";
            out << "    } else { ops = 30; }\n";
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

    out << "const { analyzeFlow } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 2201:
            out << "{\n";
            out << "    let s = [1, 2, 1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 2) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3, 4];\n";
            out << "    if (analyzeFlow(s, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 1, 1];\n";
            out << "    if (analyzeFlow(s, 1) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 1, 3, 4];\n";
            out << "    if (analyzeFlow(s, 3) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1];\n";
            out << "    if (analyzeFlow(s, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(i % 100);\n";
            out << "    if (analyzeFlow(s, 100) !== n) allCorrect = false;\n";
            out << "    if (analyzeFlow(s, 1) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 2202:
            out << "{\n";
            out << "    let s = [1, 2, 1, 3, 2];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 1, 1];\n";
            out << "    if (analyzeFlow(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3, 1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3, 4, 5];\n";
            out << "    if (analyzeFlow(s, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [7];\n";
            out << "    if (analyzeFlow(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(i % 50);\n";
            out << "    if (analyzeFlow(s, 0) !== 50) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 2203:
            out << "{\n";
            out << "    let s = [1, 2, 1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 2) !== 7) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 1, 3, 4];\n";
            out << "    if (analyzeFlow(s, 3) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 1, 1];\n";
            out << "    if (analyzeFlow(s, 1) !== 6) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 1) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(i % 10);\n";
            out << "    if (analyzeFlow(s, 1) !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 2204:
            out << "{\n";
            out << "    let s = [1, 2, 3, 1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 1, 1];\n";
            out << "    if (analyzeFlow(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3, 4, 5];\n";
            out << "    if (analyzeFlow(s, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [2, 1, 3, 2, 1];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [5];\n";
            out << "    if (analyzeFlow(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 2, 3, 4, 2, 5, 6];\n";
            out << "    if (analyzeFlow(s, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(i % 1000);\n";
            out << "    if (analyzeFlow(s, 0) !== 1000) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 21; }\n";
            break;

        case 2205:
            out << "{\n";
            out << "    let s = [3, 1, 2, 3, 7, 1, 2, 1, 3, 2, 1];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [2, 1, 2, 1, 1, 1];\n";
            out << "    if (analyzeFlow(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 5, 1, 2, 5, 3];\n";
            out << "    if (analyzeFlow(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [3, 1, 2, 3, 1, 2, 3];\n";
            out << "    if (analyzeFlow(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [2, 1, 2, 1, 1, 1, 2];\n";
            out << "    if (analyzeFlow(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let tgtSize = 100;\n";
            out << "    let streamLen = " << wave.n << ";\n";
            out << "    let s = [tgtSize];\n";
            out << "    for (let i = 1; i <= tgtSize; i++) s.push(i);\n";
            out << "    for (let i = 0; i < streamLen; i++) s.push((i % 200) + 1);\n";
            out << "    let r = analyzeFlow(s, 0);\n";
            out << "    if (r !== 100) allCorrect = false;\n";
            out << "    ops = streamLen;\n";
            out << "} else { ops = 30; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
