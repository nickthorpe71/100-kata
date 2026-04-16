#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> iron_gut::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        501, "Check the Flow", WORLD_NAME, WORLD_DESC,
        "Are the brackets in waste balanced? Each opener must match its closer in correct order. Return 1 if balanced, 0 otherwise.",
        "Example: \"([]{})\" -> 1\nExample: \"(]\" -> 0\nExample: \"\" -> 1\nExample: \"{[()]}\" -> 1\nExample: \"((())\" -> 0",
        "0 <= waste.length <= 100,000",
        100000, 1000, 300,
        "int feedTheGut(string& waste, int target)",
        "Use a stack. Push openers, pop on closers and check match. If the stack is empty at the end, the string is balanced. O(n) time and space.", generateStub, generateRunner
    });

    waves.push_back({
        502, "Digest the Expression", WORLD_NAME, WORLD_DESC,
        "Evaluate a postfix (reverse Polish) expression. Tokens are separated by spaces. Operators are + - * /. Division truncates toward zero. Return the result.",
        "Example: \"3 4 +\" -> 7\nExample: \"3 4 + 2 *\" -> 14\nExample: \"5 1 2 + 4 * + 3 -\" -> 14\nExample: \"10 3 /\" -> 3",
        "1 <= tokens <= 10,000\nAll intermediate results fit in int.\ntarget is unused.",
        10000, 1000, 300,
        "int feedTheGut(string& waste, int target)",
        "Stack of integers. Push numbers. On an operator, pop two operands, apply the operator, and push the result. The final stack top is the answer. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        503, "Simplify the Pipes", WORLD_NAME, WORLD_DESC,
        "Simplify a Unix-style path. Handle \".\", \"..\", multiple slashes. Write the simplified path back into waste. Return its length.",
        "Example: \"/a/b/../c/./d\" -> 6 (waste becomes \"/a/c/d\")\nExample: \"/home//foo/\" -> 9 (waste becomes \"/home/foo\")\nExample: \"/../\" -> 1 (waste becomes \"/\")\nExample: \"/a/../../b\" -> 2 (waste becomes \"/b\")",
        "1 <= waste.length <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int feedTheGut(string& waste, int target)",
        "Split the path by '/', use a stack of directory names. Push normal names, pop on '..', skip '.'. Rebuild the canonical path from the stack. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        504, "Next Greater Waste", WORLD_NAME, WORLD_DESC,
        "waste encodes space-separated integers. For each element, find the next greater element to its right (-1 if none). Return the sum of all next-greater values.",
        "Example: \"4 2 1 3\" -> 10 (next greater: -1,3,3,=-1 -> wait: 4->-1, 2->3, 1->3, 3->-1 = -1+3+3-1 = 4). Actually sum of next-greaters treating -1 as -1: -1+3+3+(-1) = 4\nExample: \"1 2 3\" -> 5 (2+3+(-1))\nExample: \"3 2 1\" -> -3 (-1+-1+-1)",
        "1 <= N <= 500,000\ntarget is unused.",
        500000, 1000, 300,
        "int feedTheGut(string& waste, int target)",
        "Monotonic decreasing stack. For each element, pop all smaller elements from the stack -- the current element is their next greater. Remaining stack elements have no next greater (-1). O(n).", generateStub, generateRunner
    });

    waves.push_back({
        505, "The Purge", WORLD_NAME, WORLD_DESC,
        "waste is a string of digits representing a number. Remove exactly target digits to make the remaining number as small as possible. Return that number as an int. Leading zeros are dropped. If all digits removed, return 0.",
        "Example: \"1432219\", target=3 -> 1219\nExample: \"10200\", target=1 -> 200\nExample: \"10\", target=2 -> 0\nExample: \"9\", target=1 -> 0\nExample: \"112\", target=1 -> 11",
        "1 <= waste.length <= 15\n1 <= target <= waste.length\nResult fits in int.",
        15, 1000, 300,
        "int feedTheGut(string& waste, int target)",
        "Monotonic increasing stack of digits. For each digit, pop the stack while the top is greater than current and removals remain. The result is what remains on the stack. O(n).", generateStub, generateRunner
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
            "Sliding window over candidate numbers",
            "Binary search on the answer",
            "Union-Find over digit groups"
        },
        0,
        "Need LIFO state to preserve the most recent unresolved decisions",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given a string `waste` containing bracket characters, return `1` if the brackets are balanced and `0` otherwise.";
    waves[0].flavor_text = "In the Iron Gut, a furnace diver listens to pipes knocking against one another in the dark. Every opener needs the right closer in the right order, or the furnaces below will cough the whole maze full of sparks.";
    waves[1].clear_prompt = "Given a postfix expression string `waste`, evaluate it and return the resulting integer.";
    waves[1].flavor_text = "The Iron Gut speaks in clipped tokens and old machine-chant. The diver must swallow the symbols in order, collapsing them into one final value before the glow in the pipes fades.";
    waves[2].clear_prompt = "Given a Unix-style path string `waste`, simplify it to its canonical path, write the simplified path back into `waste`, and return its length.";
    waves[2].flavor_text = "The diver crawls through maintenance shafts slick with soot, scraping away false turns, repeated slashes, and dead-end backsteps until only the true route remains.";
    waves[3].clear_prompt = "Given a sequence of integers encoded in `waste`, find each element's next greater element to its right and return the sum of those next-greater values, using `-1` where none exists.";
    waves[3].flavor_text = "Pressure builds in the furnace tunnels, each reading waiting for the next stronger surge to overtake it. The diver reads the line like a storm chart, assigning every weak pulse the first larger pulse that comes after.";
    waves[4].clear_prompt = "Given a numeric string `waste`, remove exactly `target` digits so the remaining number is as small as possible. Return the resulting integer, dropping leading zeros and returning `0` if all digits are removed.";
    waves[4].flavor_text = "The Iron Gut has locked itself with a number too proud to survive. The diver plucks away digits one by one, always choosing the cut that leaves the smallest possible residue when the heat dies down.";
    return waves;
}

void iron_gut::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int feedTheGut(string& waste, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 501:
            out << "    {\n";
            out << "        string s = \"([]{})\";\n";
            out << "        if (feedTheGut(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"(]\";\n";
            out << "        if (feedTheGut(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"\";\n";
            out << "        if (feedTheGut(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"{[()]}\";\n";
            out << "        if (feedTheGut(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"((())\";\n";
            out << "        if (feedTheGut(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \")(\";\n";
            out << "        if (feedTheGut(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"([)]\";\n";
            out << "        if (feedTheGut(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s;\n";
            out << "        for (int i = 0; i < n / 2; i++) s += '(';\n";
            out << "        for (int i = 0; i < n / 2; i++) s += ')';\n";
            out << "        if (feedTheGut(s, 0) != 1) all_correct = false;\n";
            out << "        s[n / 2] = '[';\n";
            out << "        if (feedTheGut(s, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 502:
            out << "    {\n";
            out << "        string s = \"3 4 +\";\n";
            out << "        if (feedTheGut(s, 0) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"3 4 + 2 *\";\n";
            out << "        if (feedTheGut(s, 0) != 14) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"5 1 2 + 4 * + 3 -\";\n";
            out << "        if (feedTheGut(s, 0) != 14) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10 3 /\";\n";
            out << "        if (feedTheGut(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"2 3 * 4 5 * +\";\n";
            out << "        if (feedTheGut(s, 0) != 26) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"42\";\n";
            out << "        if (feedTheGut(s, 0) != 42) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        // Build a long postfix: 1 1 + 1 + 1 + ... = n\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s = \"1\";\n";
            out << "        for (int i = 1; i < n; i++) s += \" 1 +\";\n";
            out << "        if (feedTheGut(s, 0) != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 20; }\n";
            break;

        case 503:
            out << "    {\n";
            out << "        string s = \"/a/b/../c/./d\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 6 || s.substr(0, 6) != \"/a/c/d\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"/home//foo/\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 9 || s.substr(0, 9) != \"/home/foo\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"/../\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 1 || s[0] != '/') all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"/a/../../b\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 2 || s.substr(0, 2) != \"/b\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"/\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 1 || s[0] != '/') all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"/a/b/c/d/e\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len != 10 || s.substr(0, 10) != \"/a/b/c/d/e\") all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s = \"\";\n";
            out << "        // /a/b/a/b/... repeated\n";
            out << "        for (int i = 0; i < n / 4; i++) s += \"/a/b\";\n";
            out << "        int len = feedTheGut(s, 0);\n";
            out << "        if (len < 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 20; }\n";
            break;

        case 504:
            out << "    {\n";
            out << "        string s = \"4 2 1 3\";\n";
            out << "        // next greater: 4->-1, 2->3, 1->3, 3->-1 = -1+3+3-1 = 4\n";
            out << "        if (feedTheGut(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"1 2 3\";\n";
            out << "        // 1->2, 2->3, 3->-1 = 2+3-1 = 4\n";
            out << "        if (feedTheGut(s, 0) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"3 2 1\";\n";
            out << "        // all -1 = -3\n";
            out << "        if (feedTheGut(s, 0) != -3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"5\";\n";
            out << "        if (feedTheGut(s, 0) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"1 3 2 4\";\n";
            out << "        // 1->3, 3->4, 2->4, 4->-1 = 3+4+4-1 = 10\n";
            out << "        if (feedTheGut(s, 0) != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s;\n";
            out << "        // ascending: 1 2 3 ... n\n";
            out << "        for (int i = 1; i <= n; i++) {\n";
            out << "            if (i > 1) s += \" \";\n";
            out << "            s += to_string(i);\n";
            out << "        }\n";
            out << "        // next greater for i is i+1, last is -1\n";
            out << "        // sum = 2+3+...+n + (-1) = n*(n+1)/2 - 1 - 1 = n*(n+1)/2 - 2\n";
            out << "        long long expected = (long long)n * (n + 1) / 2 - 2;\n";
            out << "        if (feedTheGut(s, 0) != (int)expected) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 505:
            out << "    {\n";
            out << "        string s = \"1432219\";\n";
            out << "        if (feedTheGut(s, 3) != 1219) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10200\";\n";
            out << "        if (feedTheGut(s, 1) != 200) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"10\";\n";
            out << "        if (feedTheGut(s, 2) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"9\";\n";
            out << "        if (feedTheGut(s, 1) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"112\";\n";
            out << "        if (feedTheGut(s, 1) != 11) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"54321\";\n";
            out << "        if (feedTheGut(s, 2) != 321) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"12345\";\n";
            out << "        if (feedTheGut(s, 2) != 123) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"100\";\n";
            out << "        if (feedTheGut(s, 1) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        // Small N for this problem (digit strings)\n";
            out << "        string s = \"987654321012\";\n";
            out << "        if (feedTheGut(s, 6) != 12) all_correct = false;\n";
            out << "        string s2 = \"111111111111\";\n";
            out << "        if (feedTheGut(s2, 6) != 111111) all_correct = false;\n";
            out << "        ops = 15;\n";
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
    std::string path = player_dir + "/runner.js";
    std::ofstream out(path);

    out << "const { feedTheGut } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 501: // Check the Flow
            out << "if (feedTheGut('([]{})', 0) !== 1) allCorrect = false;\n";
            out << "if (feedTheGut('(]', 0) !== 0) allCorrect = false;\n";
            out << "if (feedTheGut('', 0) !== 1) allCorrect = false;\n";
            out << "if (feedTheGut('{[()]}', 0) !== 1) allCorrect = false;\n";
            out << "if (feedTheGut('((())', 0) !== 0) allCorrect = false;\n";
            out << "if (feedTheGut(')(', 0) !== 0) allCorrect = false;\n";
            out << "if (feedTheGut('([)]', 0) !== 0) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = '('.repeat(n / 2) + ')'.repeat(n / 2);\n";
            out << "    if (feedTheGut(s, 0) !== 1) allCorrect = false;\n";
            out << "    s = s.substring(0, n / 2) + '[' + s.substring(n / 2 + 1);\n";
            out << "    if (feedTheGut(s, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 502: // Digest the Expression
            out << "if (feedTheGut('3 4 +', 0) !== 7) allCorrect = false;\n";
            out << "if (feedTheGut('3 4 + 2 *', 0) !== 14) allCorrect = false;\n";
            out << "if (feedTheGut('5 1 2 + 4 * + 3 -', 0) !== 14) allCorrect = false;\n";
            out << "if (feedTheGut('10 3 /', 0) !== 3) allCorrect = false;\n";
            out << "if (feedTheGut('2 3 * 4 5 * +', 0) !== 26) allCorrect = false;\n";
            out << "if (feedTheGut('42', 0) !== 42) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = '1';\n";
            out << "    for (let i = 1; i < n; i++) s += ' 1 +';\n";
            out << "    if (feedTheGut(s, 0) !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 20; }\n";
            break;

        case 503: // Simplify the Pipes
            out << "{\n";
            out << "    let r = feedTheGut('/a/b/../c/./d', 0);\n";
            out << "    if (r !== '/a/c/d') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = feedTheGut('/home//foo/', 0);\n";
            out << "    if (r !== '/home/foo') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = feedTheGut('/../', 0);\n";
            out << "    if (r !== '/') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = feedTheGut('/a/../../b', 0);\n";
            out << "    if (r !== '/b') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = feedTheGut('/', 0);\n";
            out << "    if (r !== '/') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let r = feedTheGut('/a/b/c/d/e', 0);\n";
            out << "    if (r !== '/a/b/c/d/e') allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = '';\n";
            out << "    for (let i = 0; i < Math.floor(n / 4); i++) s += '/a/b';\n";
            out << "    let r = feedTheGut(s, 0);\n";
            out << "    if (r.length < 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 20; }\n";
            break;

        case 504: // Next Greater Waste
            out << "if (feedTheGut('4 2 1 3', 0) !== 4) allCorrect = false;\n";
            out << "if (feedTheGut('1 2 3', 0) !== 4) allCorrect = false;\n";
            out << "if (feedTheGut('3 2 1', 0) !== -3) allCorrect = false;\n";
            out << "if (feedTheGut('5', 0) !== -1) allCorrect = false;\n";
            out << "if (feedTheGut('1 3 2 4', 0) !== 10) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let parts = [];\n";
            out << "    for (let i = 1; i <= n; i++) parts.push(String(i));\n";
            out << "    let s = parts.join(' ');\n";
            out << "    let expected = n * (n + 1) / 2 - 2;\n";
            out << "    if (feedTheGut(s, 0) !== expected) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 505: // The Purge (boss)
            out << "if (feedTheGut('1432219', 3) !== 1219) allCorrect = false;\n";
            out << "if (feedTheGut('10200', 1) !== 200) allCorrect = false;\n";
            out << "if (feedTheGut('10', 2) !== 0) allCorrect = false;\n";
            out << "if (feedTheGut('9', 1) !== 0) allCorrect = false;\n";
            out << "if (feedTheGut('112', 1) !== 11) allCorrect = false;\n";
            out << "if (feedTheGut('54321', 2) !== 321) allCorrect = false;\n";
            out << "if (feedTheGut('12345', 2) !== 123) allCorrect = false;\n";
            out << "if (feedTheGut('100', 1) !== 0) allCorrect = false;\n";
            out << "if (!testOnly) {\n";
            out << "    if (feedTheGut('987654321012', 6) !== 12) allCorrect = false;\n";
            out << "    if (feedTheGut('111111111111', 6) !== 111111) allCorrect = false;\n";
            out << "    ops = 15;\n";
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
