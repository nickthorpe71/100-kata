#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> binary_telegraph::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1901, "Count the Pulses", WORLD_NAME, WORLD_DESC,
        "Count the total number of set bits (1s) across ALL elements in signals. target is unused.",
        "Example: [7,3,1] -> 6 (111=3, 11=2, 1=1)\nExample: [0] -> 0\nExample: [15] -> 4\nExample: [1,1,1,1] -> 4",
        "0 <= signals[i] <= 2^31 - 1\n1 <= N <= 100,000",
        100000, 1000, 300,
        "int tapTheWire(vector<int>& signals, int target)",
        "For each number, count set bits using n & (n-1) trick (Brian Kernighan's algorithm -- each iteration clears lowest set bit). Sum across all elements. O(n * bits).", generateStub, generateRunner
    });

    waves.push_back({
        1902, "The Lone Signal", WORLD_NAME, WORLD_DESC,
        "Every element in signals appears exactly twice except one. Find and return the lone element. target is unused.",
        "Example: [2,2,1] -> 1\nExample: [4,1,2,1,2] -> 4\nExample: [1] -> 1\nExample: [0,1,0] -> 1",
        "1 <= N <= 100,001 (always odd)\nExactly one element appears once.",
        100001, 1000, 300,
        "int tapTheWire(vector<int>& signals, int target)",
        "XOR all elements together. Since a^a=0 and a^0=a, all pairs cancel out, leaving the lone element. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1903, "Power Check", WORLD_NAME, WORLD_DESC,
        "Return 1 if target is a power of 2, 0 otherwise. signals is unused.",
        "Example: target=1 -> 1\nExample: target=16 -> 1\nExample: target=0 -> 0\nExample: target=6 -> 0\nExample: target=1024 -> 1",
        "0 <= target <= 2^30\nsignals is unused.",
        1, 1000, 300,
        "int tapTheWire(vector<int>& signals, int target)",
        "A power of 2 has exactly one set bit. Check: n > 0 && (n & (n-1)) == 0. O(1).", generateStub, generateRunner
    });

    waves.push_back({
        1904, "Reverse the Bits", WORLD_NAME, WORLD_DESC,
        "Count the number of bits needed to represent target (position of highest set bit), then reverse those bits. Return the result. If target is 0, return 0.",
        "Example: target=13 (1101) -> 11 (1011)\nExample: target=6 (110) -> 3 (011)\nExample: target=1 -> 1\nExample: target=0 -> 0\nExample: target=8 (1000) -> 1 (0001)",
        "0 <= target <= 2^30\nsignals is unused.",
        1, 1000, 300,
        "int tapTheWire(vector<int>& signals, int target)",
        "Find the number of significant bits. Then reverse by building result bit by bit -- extract LSB of target, shift into result from the other end. O(bits).", generateStub, generateRunner
    });

    waves.push_back({
        1905, "The Missing Pulse", WORLD_NAME, WORLD_DESC,
        "signals contains numbers 0 to N with exactly one missing. Return the missing number. target is unused.",
        "Example: [3,0,1] -> 2\nExample: [0,1] -> 2\nExample: [9,6,4,2,3,5,7,0,1] -> 8\nExample: [0] -> 1",
        "0 <= signals[i] <= N\nN = signals.size()\nExactly one number in [0..N] is missing.",
        500000, 1000, 300,
        "int tapTheWire(vector<int>& signals, int target)",
        "XOR all values with all indices 0..n. The missing number is the only unpaired value and remains after all XORs. O(n). Alternative: sum formula n*(n+1)/2 minus actual sum.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a bit-manipulation approach",
        "Need direct bit-level operations to count, isolate, or cancel state",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Bitmask accumulation over each word and pairwise comparison",
            "Sliding window over the words",
            "Union-Find over shared letters",
            "Heap of largest masks"
        },
        0,
        "Need direct bit-level operations to count, isolate, or cancel state",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given an array `signals`, return the total number of set bits across all integers in the array.";
    waves[0].flavor_text = "Telegraph keeper Orrin leans over the humming relay wall and counts every lit pulse rushing through the copper wire. Each bit is a spark, and the total brightness matters more than any single fragment of message.";
    waves[1].clear_prompt = "Given an array `signals` where every value appears twice except one, return the value that appears once.";
    waves[1].flavor_text = "Most pulses on the line arrive in mirrored pairs and cancel into clean noise. Orrin is hunting the one signal that lacks a twin, the solitary message still hiding in the static.";
    waves[2].clear_prompt = "Given integer `target`, return `1` if it is a power of two and `0` otherwise.";
    waves[2].flavor_text = "The telegraph's amplifiers only resonate with pure doubling rhythms. Orrin tests the incoming value and asks a simple question: is this signal a true tower of twos, or only an imitation close enough to fool the careless?";
    waves[3].clear_prompt = "Given integer `target`, reverse its relevant bits as defined by the wave and return the resulting integer.";
    waves[3].flavor_text = "A coded pulse has arrived backward, like a warning reflected through cracked glass. Orrin must flip the bit pattern end to end before the message can be trusted by anyone on the night shift.";
    waves[4].clear_prompt = "Given an array of strings or bit-encodable values as defined by the wave, return the required maximum value using bitmask comparison.";
    waves[4].flavor_text = "Across the relay wall, Orrin compares whole words as fields of light and shadow. Some pairings interfere, some harmonize, and one hidden match produces the strongest clean signal the machine can deliver.";
    return waves;
}

void binary_telegraph::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int tapTheWire(vector<int>& signals, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1901: // Count the Pulses
            out << "    {\n";
            out << "        vector<int> s = {7, 3, 1};\n";
            out << "        if (tapTheWire(s, 0) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {15};\n";
            out << "        if (tapTheWire(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 1, 1, 1};\n";
            out << "        if (tapTheWire(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {255};\n";
            out << "        if (tapTheWire(s, 0) != 8) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s(n, 7); // each has 3 set bits\n";
            out << "        if (tapTheWire(s, 0) != 3 * n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 10; }\n";
            break;

        case 1902: // The Lone Signal
            out << "    {\n";
            out << "        vector<int> s = {2, 2, 1};\n";
            out << "        if (tapTheWire(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {4, 1, 2, 1, 2};\n";
            out << "        if (tapTheWire(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1};\n";
            out << "        if (tapTheWire(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0, 1, 0};\n";
            out << "        if (tapTheWire(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {5, 3, 5, 3, 99};\n";
            out << "        if (tapTheWire(s, 0) != 99) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> s;\n";
            out << "        for (int i = 0; i < n / 2; i++) { s.push_back(i); s.push_back(i); }\n";
            out << "        s.push_back(999999);\n";
            out << "        if (tapTheWire(s, 0) != 999999) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 1903: // Power Check
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 16) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 6) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 1024) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 3) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 1073741824) != 1) all_correct = false; // 2^30\n";
            out << "        if (tapTheWire(s, 1073741823) != 0) all_correct = false; // 2^30 - 1\n";
            out << "        ops = 2;\n";
            out << "    } else { ops = 7; }\n";
            break;

        case 1904: // Reverse the Bits
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 13) != 11) all_correct = false; // 1101 -> 1011\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 6) != 3) all_correct = false; // 110 -> 011 = 3\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 8) != 1) all_correct = false; // 1000 -> 0001\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 5) != 5) all_correct = false; // 101 -> 101 (palindrome)\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 10) != 5) all_correct = false; // 1010 -> 0101 = 5\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> s = {0};\n";
            out << "        // 1073741824 = 2^30 = 1 followed by 30 zeros -> reversed = 1 (0...01)\n";
            out << "        if (tapTheWire(s, 1073741824) != 1) all_correct = false;\n";
            out << "        // 536870912 = 2^29 -> reversed = 1\n";
            out << "        if (tapTheWire(s, 536870912) != 1) all_correct = false;\n";
            out << "        ops = 2;\n";
            out << "    } else { ops = 7; }\n";
            break;

        case 1905: // The Missing Pulse (boss)
            out << "    {\n";
            out << "        vector<int> s = {3, 0, 1};\n";
            out << "        if (tapTheWire(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0, 1};\n";
            out << "        if (tapTheWire(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {9, 6, 4, 2, 3, 5, 7, 0, 1};\n";
            out << "        if (tapTheWire(s, 0) != 8) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {0};\n";
            out << "        if (tapTheWire(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1, 0, 3};\n";
            out << "        if (tapTheWire(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> s = {1};\n";
            out << "        if (tapTheWire(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        int missing = n / 2;\n";
            out << "        vector<int> s;\n";
            out << "        for (int i = 0; i <= n; i++) { if (i != missing) s.push_back(i); }\n";
            out << "        if (tapTheWire(s, 0) != missing) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 20; }\n";
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

    out << "const { tapTheWire } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1901: // Count the Pulses
            out << "{\n";
            out << "    let s = [7, 3, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 6) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [0];\n";
            out << "    if (tapTheWire(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [15];\n";
            out << "    if (tapTheWire(s, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 1, 1, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [255];\n";
            out << "    if (tapTheWire(s, 0) !== 8) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = Array(n).fill(7); // each has 3 set bits\n";
            out << "    if (tapTheWire(s, 0) !== 3 * n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 10; }\n";
            break;

        case 1902: // The Lone Signal
            out << "{\n";
            out << "    let s = [2, 2, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [4, 1, 2, 1, 2];\n";
            out << "    if (tapTheWire(s, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1];\n";
            out << "    if (tapTheWire(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [0, 1, 0];\n";
            out << "    if (tapTheWire(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [5, 3, 5, 3, 99];\n";
            out << "    if (tapTheWire(s, 0) !== 99) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < Math.floor(n / 2); i++) { s.push(i); s.push(i); }\n";
            out << "    s.push(999999);\n";
            out << "    if (tapTheWire(s, 0) !== 999999) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 1903: // Power Check
            out << "if (tapTheWire([0], 1) !== 1) allCorrect = false;\n";
            out << "if (tapTheWire([0], 16) !== 1) allCorrect = false;\n";
            out << "if (tapTheWire([0], 0) !== 0) allCorrect = false;\n";
            out << "if (tapTheWire([0], 6) !== 0) allCorrect = false;\n";
            out << "if (tapTheWire([0], 1024) !== 1) allCorrect = false;\n";
            out << "if (tapTheWire([0], 2) !== 1) allCorrect = false;\n";
            out << "if (tapTheWire([0], 3) !== 0) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    if (tapTheWire([0], 1073741824) !== 1) allCorrect = false; // 2^30\n";
            out << "    if (tapTheWire([0], 1073741823) !== 0) allCorrect = false; // 2^30 - 1\n";
            out << "    ops = 2;\n";
            out << "} else { ops = 7; }\n";
            break;

        case 1904: // Reverse the Bits
            out << "if (tapTheWire([0], 13) !== 11) allCorrect = false; // 1101 -> 1011\n";
            out << "if (tapTheWire([0], 6) !== 3) allCorrect = false; // 110 -> 011 = 3\n";
            out << "if (tapTheWire([0], 1) !== 1) allCorrect = false;\n";
            out << "if (tapTheWire([0], 0) !== 0) allCorrect = false;\n";
            out << "if (tapTheWire([0], 8) !== 1) allCorrect = false; // 1000 -> 0001\n";
            out << "if (tapTheWire([0], 5) !== 5) allCorrect = false; // 101 -> 101 (palindrome)\n";
            out << "if (tapTheWire([0], 10) !== 5) allCorrect = false; // 1010 -> 0101 = 5\n";
            out << "if (!testOnly) {\n";
            out << "    // 1073741824 = 2^30 = 1 followed by 30 zeros -> reversed = 1 (0...01)\n";
            out << "    if (tapTheWire([0], 1073741824) !== 1) allCorrect = false;\n";
            out << "    // 536870912 = 2^29 -> reversed = 1\n";
            out << "    if (tapTheWire([0], 536870912) !== 1) allCorrect = false;\n";
            out << "    ops = 2;\n";
            out << "} else { ops = 7; }\n";
            break;

        case 1905: // The Missing Pulse (boss)
            out << "{\n";
            out << "    let s = [3, 0, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [0, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [9, 6, 4, 2, 3, 5, 7, 0, 1];\n";
            out << "    if (tapTheWire(s, 0) !== 8) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [0];\n";
            out << "    if (tapTheWire(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1, 0, 3];\n";
            out << "    if (tapTheWire(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [1];\n";
            out << "    if (tapTheWire(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let missing = Math.floor(n / 2);\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i <= n; i++) { if (i !== missing) s.push(i); }\n";
            out << "    if (tapTheWire(s, 0) !== missing) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 20; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
