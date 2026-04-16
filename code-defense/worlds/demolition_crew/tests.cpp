#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>
#include <filesystem>

std::vector<WaveDef> demolition_crew::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2401, "Minimize the Skyline", WORLD_NAME, WORLD_DESC,
        "Remove exactly target characters from the digit string skyline to make the remaining number as small as possible. Return that number. Leading zeros are dropped. If all digits removed, return 0.",
        "Example: \"1432219\", target=3 -> 1219\nExample: \"10200\", target=1 -> 200\nExample: \"10\", target=2 -> 0\nExample: \"54321\", target=2 -> 321",
        "1 <= skyline.length <= 15\n1 <= target <= skyline.length\nResult fits in int.",
        15, 1000, 300,
        "int demolish(string& skyline, int target)",
        "Monotonic increasing stack of digits. For each new digit, pop stack while top > current and removals remain. Push current digit. After processing, remove from end if removals remain. Strip leading zeros. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2402, "Unique Demolition", WORLD_NAME, WORLD_DESC,
        "Remove duplicate characters so each appears only once, keeping the smallest lexicographic result. Write the result back into skyline. Return the length of the result string. target is unused.",
        "Example: \"bcabc\" -> 3 (skyline becomes \"abc\")\nExample: \"cbacdcbc\" -> 4 (skyline becomes \"acdb\")\nExample: \"a\" -> 1\nExample: \"aaa\" -> 1 (skyline becomes \"a\")",
        "1 <= skyline.length <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int demolish(string& skyline, int target)",
        "Stack-based greedy. Track last occurrence of each character and whether it's in the result. For each character, while stack top > current and stack top appears later, pop it. Push current if not already in result. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        2403, "Decode the Blueprint", WORLD_NAME, WORLD_DESC,
        "Decode an encoded string like \"3[ab]2[c]\" -> \"abababcc\". Handles nesting: \"2[a3[b]]\" -> \"abbbabbb\". Write the decoded string into skyline. Return its length. target is unused.",
        "Example: \"3[a]2[bc]\" -> 7 (skyline becomes \"aaabcbc\")\nExample: \"2[a3[b]]\" -> 8 (skyline becomes \"abbbabbb\")\nExample: \"abc\" -> 3\nExample: \"10[a]\" -> 10",
        "1 <= skyline.length <= 100,000\nDecoded length <= 1,000,000\ntarget is unused.",
        100000, 1000, 300,
        "int demolish(string& skyline, int target)",
        "Stack of (current_string, repeat_count). On '[', push current state and start fresh. On ']', pop and append current string repeated. On digit, accumulate the number. On letter, append to current string. O(output_length).", generateStub, generateRunner
    });

    waves.push_back({
        2404, "Asteroid Field", WORLD_NAME, WORLD_DESC,
        "skyline encodes space-separated integers (positive = moving right, negative = moving left). Asteroids moving in opposite directions collide — the larger survives, equal size both explode. Return the count of surviving asteroids. target is unused.",
        "Example: \"5 10 -5\" -> 2 (5 and 10 survive, -5 destroyed by 10)\nExample: \"8 -8\" -> 0 (equal, both explode)\nExample: \"10 2 -5\" -> 1 (10 survives)\nExample: \"-1 -2 1 2\" -> 4 (no collisions, moving apart)",
        "1 <= number of asteroids <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int demolish(string& skyline, int target)",
        "Stack simulation. Process left to right. Positive asteroids push to stack. Negative asteroids collide with stack top if positive. Larger survives, equal both die. O(n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a stack-based approach",
        "Need LIFO state to preserve the most recent unresolved decisions",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Monotonic stack of digits with controlled removals",
            "Sliding window of candidate numbers",
            "Binary search on the answer",
            "Union-Find over digit groups"
        },
        0,
        "Need LIFO state to preserve the most recent unresolved decisions",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given a digit string `skyline` and integer `target`, remove exactly `target` digits so the remaining number is as small as possible. Return the resulting integer.";
    waves[0].flavor_text = "The demolition chief studies the skyline in silhouette, deciding which marked towers must fall so the horizon settles into its smallest possible shape. Every removed digit changes what the surviving city can become.";
    waves[1].clear_prompt = "Given a string `skyline`, remove duplicate characters so each appears once while keeping the lexicographically smallest possible result. Write the result back into `skyline` and return its length.";
    waves[1].flavor_text = "The blast marks on the blueprint are redundant and ugly. The chief strips repeated symbols from the plan, but only in a way that leaves the final inscription as small and clean as possible.";
    waves[2].clear_prompt = "Given an encoded string `skyline`, decode it according to the repetition rules described by the wave, write the decoded result back into `skyline`, and return its length.";
    waves[2].flavor_text = "Blueprints arrive in layered brackets and counts, like instructions whispered through collapsed walls. The chief must unfold them exactly before the crew can touch the structure.";
    waves[3].clear_prompt = "Given space-separated signed integers in `skyline`, simulate asteroid collisions and return the number of surviving asteroids.";
    waves[3].flavor_text = "In the blast tunnel, stone masses rush toward each other under broken floodlights. The chief predicts each collision in sequence, watching the debris field simplify into whatever survives the impacts.";
    return waves;
}

void demolition_crew::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <string>\n";
    out << "#include <vector>\n";
    out << "#include <stack>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <algorithm>\n";
    out << "using namespace std;\n\n";

    out << "int demolish(string& skyline, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 2401: // Minimize the Skyline
            out << "    {\n";
            out << "        string s = \"1432219\";\n";
            out << "        if (demolish(s, 3) != 1219) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10200\";\n";
            out << "        if (demolish(s, 1) != 200) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10\";\n";
            out << "        if (demolish(s, 2) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"54321\";\n";
            out << "        if (demolish(s, 2) != 321) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"9\";\n";
            out << "        if (demolish(s, 1) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"112\";\n";
            out << "        if (demolish(s, 1) != 11) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        string s = \"987654321012345\";\n";
            out << "        if (demolish(s, 7) != 1012345) all_correct = false;\n";
            out << "        string s2 = \"111111111111111\";\n";
            out << "        if (demolish(s2, 7) != 11111111) all_correct = false;\n";
            out << "        ops = 15;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 2402: // Unique Demolition
            out << "    {\n";
            out << "        string s = \"bcabc\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 3 || s.substr(0, 3) != \"abc\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"cbacdcbc\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 4 || s.substr(0, 4) != \"acdb\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"a\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 1 || s[0] != 'a') all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"aaa\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 1 || s[0] != 'a') all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"abacb\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 3 || s.substr(0, 3) != \"abc\") all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s;\n";
            out << "        for (int i = 0; i < n; i++) s += 'a' + (i % 26);\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 26) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 2403: // Decode the Blueprint
            out << "    {\n";
            out << "        string s = \"3[a]2[bc]\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 7 || s.substr(0, 7) != \"aaabcbc\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"2[a3[b]]\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 8 || s.substr(0, 8) != \"abbbabbb\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"abc\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 3 || s.substr(0, 3) != \"abc\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10[a]\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 10 || s.substr(0, 10) != \"aaaaaaaaaa\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"2[ab3[c]]\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 10 || s.substr(0, 10) != \"abcccabccc\") all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        // Deeply nested: 2[2[2[2[ab]]]] = 16 * \"ab\" = 32 chars\n";
            out << "        string s = \"2[2[2[2[ab]]]]\";\n";
            out << "        int len = demolish(s, 0);\n";
            out << "        if (len != 32) all_correct = false;\n";
            out << "        // Wider: 3[a]3[b]3[c]3[d]\n";
            out << "        string s2 = \"3[a]3[b]3[c]3[d]\";\n";
            out << "        int len2 = demolish(s2, 0);\n";
            out << "        if (len2 != 12 || s2.substr(0, 12) != \"aaabbbcccddd\") all_correct = false;\n";
            out << "        ops = 100;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 2404: // Asteroid Field (boss)
            out << "    {\n";
            out << "        string s = \"5 10 -5\";\n";
            out << "        if (demolish(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"8 -8\";\n";
            out << "        if (demolish(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10 2 -5\";\n";
            out << "        if (demolish(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"-1 -2 1 2\";\n";
            out << "        if (demolish(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"-2 -1 1 2\";\n";
            out << "        if (demolish(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"1 -1 1 -1\";\n";
            out << "        if (demolish(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"-1 -2 -3\";\n";
            out << "        if (demolish(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"1 2 3\";\n";
            out << "        if (demolish(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        // All positive — no collisions, all survive\n";
            out << "        string s;\n";
            out << "        for (int i = 0; i < n; i++) {\n";
            out << "            if (i > 0) s += \" \";\n";
            out << "            s += to_string(i + 1);\n";
            out << "        }\n";
            out << "        if (demolish(s, 0) != n) all_correct = false;\n";
            out << "        // Alternating: big positive then small negative — negatives destroyed\n";
            out << "        string s2;\n";
            out << "        for (int i = 0; i < n; i++) {\n";
            out << "            if (i > 0) s2 += \" \";\n";
            out << "            if (i % 2 == 0) s2 += to_string(n);\n";
            out << "            else s2 += to_string(-1);\n";
            out << "        }\n";
            out << "        int expected = (n + 1) / 2; // only the positives survive\n";
            out << "        if (demolish(s2, 0) != expected) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 22; }\n";
            break;
    }

    out << "\n    auto end = chrono::high_resolution_clock::now();\n";
    out << "    int ms = (int)chrono::duration_cast<chrono::milliseconds>(end - start).count();\n\n";
    out << "    printf(\"%d %d\\n\", ms, ops);\n";
    out << "    return all_correct ? 0 : 1;\n";
    out << "}\n";

    out.close();
  } else {
    // JavaScript runner
    std::string path = player_dir + "/runner.js";
    std::ofstream out(path);

    out << "const { demolish } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 2401: // Minimize the Skyline
            out << "{\n";
            out << "    let s = \"1432219\";\n";
            out << "    if (demolish(s, 3) !== 1219) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"10200\";\n";
            out << "    if (demolish(s, 1) !== 200) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"10\";\n";
            out << "    if (demolish(s, 2) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"54321\";\n";
            out << "    if (demolish(s, 2) !== 321) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"9\";\n";
            out << "    if (demolish(s, 1) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"112\";\n";
            out << "    if (demolish(s, 1) !== 11) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let s = \"987654321012345\";\n";
            out << "    if (demolish(s, 7) !== 1012345) allCorrect = false;\n";
            out << "    let s2 = \"111111111111111\";\n";
            out << "    if (demolish(s2, 7) !== 11111111) allCorrect = false;\n";
            out << "    ops = 15;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 2402: // Unique Demolition
            // In JS, demolish returns the result string; check length and content
            out << "{\n";
            out << "    let s = \"bcabc\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 3 || res !== \"abc\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"cbacdcbc\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 4 || res !== \"acdb\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"a\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 1 || res !== \"a\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"aaa\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 1 || res !== \"a\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"abacb\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 3 || res !== \"abc\") allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    const n = " << wave.n << ";\n";
            out << "    let s = '';\n";
            out << "    for (let i = 0; i < n; i++) s += String.fromCharCode(97 + (i % 26));\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 26) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 2403: // Decode the Blueprint
            // In JS, demolish returns the decoded string; check length and content
            out << "{\n";
            out << "    let s = \"3[a]2[bc]\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 7 || res !== \"aaabcbc\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"2[a3[b]]\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 8 || res !== \"abbbabbb\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"abc\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 3 || res !== \"abc\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"10[a]\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 10 || res !== \"aaaaaaaaaa\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"2[ab3[c]]\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 10 || res !== \"abcccabccc\") allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    // Deeply nested: 2[2[2[2[ab]]]] = 16 * \"ab\" = 32 chars\n";
            out << "    let s = \"2[2[2[2[ab]]]]\";\n";
            out << "    let res = demolish(s, 0);\n";
            out << "    if (res.length !== 32) allCorrect = false;\n";
            out << "    // Wider: 3[a]3[b]3[c]3[d]\n";
            out << "    let s2 = \"3[a]3[b]3[c]3[d]\";\n";
            out << "    let res2 = demolish(s2, 0);\n";
            out << "    if (res2.length !== 12 || res2 !== \"aaabbbcccddd\") allCorrect = false;\n";
            out << "    ops = 100;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 2404: // Asteroid Field (boss)
            out << "{\n";
            out << "    let s = \"5 10 -5\";\n";
            out << "    if (demolish(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"8 -8\";\n";
            out << "    if (demolish(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"10 2 -5\";\n";
            out << "    if (demolish(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"-1 -2 1 2\";\n";
            out << "    if (demolish(s, 0) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"-2 -1 1 2\";\n";
            out << "    if (demolish(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"1 -1 1 -1\";\n";
            out << "    if (demolish(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"-1 -2 -3\";\n";
            out << "    if (demolish(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"1 2 3\";\n";
            out << "    if (demolish(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    const n = " << wave.n << ";\n";
            out << "    // All positive — no collisions, all survive\n";
            out << "    let parts = [];\n";
            out << "    for (let i = 0; i < n; i++) parts.push(String(i + 1));\n";
            out << "    let s = parts.join(' ');\n";
            out << "    if (demolish(s, 0) !== n) allCorrect = false;\n";
            out << "    // Alternating: big positive then small negative — negatives destroyed\n";
            out << "    let parts2 = [];\n";
            out << "    for (let i = 0; i < n; i++) {\n";
            out << "        if (i % 2 === 0) parts2.push(String(n));\n";
            out << "        else parts2.push(String(-1));\n";
            out << "    }\n";
            out << "    let s2 = parts2.join(' ');\n";
            out << "    const expected = Math.floor((n + 1) / 2); // only the positives survive\n";
            out << "    if (demolish(s2, 0) !== expected) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 22; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
