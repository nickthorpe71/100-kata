#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> adepts_liturgy::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        201, "Scan the Corruption", WORLD_NAME, WORLD_DESC,
        "Count how many times the corrupted glyph (target as char) appears in the codex.",
        "Example: \"aababc\", target='a' -> 3\nExample: \"hello\", target='z' -> 0\nExample: \"xxx\", target='x' -> 3",
        "1 <= codex.length <= 100,000",
        100000, 1000, 300,
        "int restoreLiturgy(string& codex, int target)",
        "Loop through the string and increment a counter each time a character matches the target. O(n) time, O(1) space. A single linear scan is all you need.", generateStub, generateRunner
    });

    waves.push_back({
        202, "Purge the Glyph", WORLD_NAME, WORLD_DESC,
        "Remove all occurrences of the corrupted glyph (target as char) from the codex in-place. Return the new length. Remaining characters must preserve their original order.",
        "Example: \"aababc\", target='a' -> 3 (codex becomes \"bbc...\")\nExample: \"xxx\", target='x' -> 0\nExample: \"abc\", target='z' -> 3",
        "1 <= codex.length <= 100,000",
        100000, 1000, 300,
        "int restoreLiturgy(string& codex, int target)",
        "Use the write-pointer technique. Scan with a read pointer; when a character does not match the target, copy it to the write position and advance both pointers. Skip matching characters. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        203, "Invert the Verse", WORLD_NAME, WORLD_DESC,
        "The verse was inscribed backwards. Reverse the codex in place. Return 0. Target is unused.",
        "Example: \"abcde\" -> \"edcba\"\nExample: \"a\" -> \"a\"\nExample: \"ab\" -> \"ba\"",
        "1 <= codex.length <= 200,000\ntarget is unused.",
        200000, 1000, 300,
        "int restoreLiturgy(string& codex, int target)",
        "Place two pointers at opposite ends of the string and swap them, moving inward until they meet. O(n) time, O(1) space. The key insight is that reversing in-place only requires swapping the first with the last, the second with the second-to-last, and so on.", generateStub, generateRunner
    });

    waves.push_back({
        204, "Compress the Litany", WORLD_NAME, WORLD_DESC,
        "Run-length encode the codex in place: \"aaabbc\" becomes \"a3b2c1\". Return the new length. Every character gets a count, even if it's 1. Counts are single digits (guaranteed <= 9 consecutive).",
        "Example: \"aaabbc\" -> \"a3b2c1\", return 6\nExample: \"abc\" -> \"a1b1c1\", return 6\nExample: \"aaa\" -> \"a3\", return 2",
        "1 <= codex.length <= 100,000\nNo run exceeds 9 consecutive characters.\nThe encoded form is guaranteed to fit in the original string's allocated space.",
        100000, 1000, 300,
        "int restoreLiturgy(string& codex, int target)",
        "Track the current character and its consecutive run count. When the character changes, write the char and its count digit to the output position. The encoded form is guaranteed to fit in-place because every run gets exactly 2 characters and no run exceeds 9. O(n) time, O(1) space.", generateStub, generateRunner
    });

    waves.push_back({
        205, "Verify the Canon", WORLD_NAME, WORLD_DESC,
        "The canon is the sacred reference text. Return 1 if codex and canon are anagrams (same characters, same counts, possibly different order). Return 0 otherwise. Parameter canon is added this wave.",
        "Example: \"listen\", \"silent\" -> 1\nExample: \"hello\", \"world\" -> 0\nExample: \"aab\", \"aba\" -> 1\nExample: \"ab\", \"abc\" -> 0",
        "1 <= codex.length, canon.length <= 200,000\ntarget is unused.",
        200000, 1000, 300,
        "int restoreLiturgy(string& codex, string& canon, int target)",
        "Count character frequencies using a fixed-size array (size 26 for lowercase, or 256 for all ASCII). Build frequency counts for both strings, then compare the arrays. If they match, the strings are anagrams. O(n) time, O(1) space since the alphabet size is constant.", generateStub, generateRunner
    });

    waves.push_back({
        206, "The Cyclic Prayer", WORLD_NAME, WORLD_DESC,
        "The prayer is cyclic — it can start at any point and wrap around. Return 1 if canon is a rotation of codex (e.g. \"cdeab\" is a rotation of \"abcde\"). Return 0 otherwise. Hint: if you concatenate codex with itself, any rotation appears as a substring.",
        "Example: \"abcde\", \"cdeab\" -> 1\nExample: \"abcde\", \"abced\" -> 0\nExample: \"a\", \"a\" -> 1\nExample: \"ab\", \"ba\" -> 1",
        "1 <= codex.length, canon.length <= 500,000",
        500000, 500, 300,
        "int restoreLiturgy(string& codex, string& canon, int target)",
        "Concatenate codex with itself. Any valid rotation of codex will appear as a contiguous substring in the doubled string. Check if canon is a substring of codex+codex (and that lengths match). O(n) time using the built-in string find.", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a string traversal / transformation approach",
        "Need ordered character-by-character scanning or in-place text updates",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "String doubling plus substring search",
            "Sliding window with counts",
            "Trie with prefix nodes",
            "Dynamic programming over suffixes"
        },
        0,
        "Need ordered character-by-character scanning or in-place text updates",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given a string `codex` and a target character encoded in `target`, return how many times that character appears in the string.";
    waves[0].flavor_text = "Sister Vael, the temple's youngest archivist, reads the damaged liturgy by candle smoke and thumbnail. One heretical glyph keeps resurfacing in the chant, and she must count every appearance before the wrong verse spreads through the choir.";
    waves[1].clear_prompt = "Given a string `codex` and a target character encoded in `target`, remove all occurrences of that character in place and return the new logical length.";
    waves[1].flavor_text = "The codex has been stained by a forbidden hand. Vael must scrape every corrupted character from the line and press the surviving letters back together so the prayer can still be carried into the crypt.";
    waves[2].clear_prompt = "Given a string `codex`, reverse it in place and return `0`.";
    waves[2].flavor_text = "A page of the liturgy was copied in reverse during a midnight panic. Vael turns the verse back on itself until the sacred words face the world in their proper order again.";
    waves[3].clear_prompt = "Given a string `codex`, run-length encode it in place as described by the wave and return the new logical length.";
    waves[3].flavor_text = "The old prayer is too long to memorize before the descent begins. Vael compresses repeated sounds into tighter marks, shrinking the chant into a form the adepts can carry underground.";
    waves[4].clear_prompt = "Given two strings `codex` and `canon`, return `1` if they are anagrams and `0` otherwise.";
    waves[4].flavor_text = "The living chant from the nave no longer matches the official canon kept under glass. Vael must decide whether the difference is only arrangement and breath, or whether the choir has truly drifted into heresy.";
    waves[5].clear_prompt = "Given two strings `codex` and `canon`, return `1` if `canon` is a rotation of `codex` and `0` otherwise.";
    waves[5].flavor_text = "The prayer wheel has shifted the chant to a strange new starting point. Vael must tell whether this is a new rite entirely or only the same sacred circle entered from another spoke.";
    return waves;
}

void adepts_liturgy::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <string>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <algorithm>\n";
    out << "using namespace std;\n\n";

    if (wave.id <= 204) {
        out << "int restoreLiturgy(string& codex, int target);\n\n";
    } else {
        out << "int restoreLiturgy(string& codex, string& canon, int target);\n\n";
    }

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 201: // Scan the Corruption
            out << "    {\n";
            out << "        string s = \"aababc\";\n";
            out << "        if (restoreLiturgy(s, 'a') != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"hello\";\n";
            out << "        if (restoreLiturgy(s, 'z') != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"xxx\";\n";
            out << "        if (restoreLiturgy(s, 'x') != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"a\";\n";
            out << "        if (restoreLiturgy(s, 'a') != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) s[i] = 'a' + (i % 26);\n";
            out << "        // 'a' appears ceil(n/26) times\n";
            out << "        int expected = (n + 25) / 26;\n";
            out << "        if (restoreLiturgy(s, 'a') != expected) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 10; }\n";
            break;

        case 202: // Purge the Glyph
            out << "    {\n";
            out << "        string s = \"aababc\";\n";
            out << "        int len = restoreLiturgy(s, 'a');\n";
            out << "        if (len != 3 || s[0] != 'b' || s[1] != 'b' || s[2] != 'c') all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"xxx\";\n";
            out << "        int len = restoreLiturgy(s, 'x');\n";
            out << "        if (len != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"abc\";\n";
            out << "        int len = restoreLiturgy(s, 'z');\n";
            out << "        if (len != 3 || s.substr(0, 3) != \"abc\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"abacada\";\n";
            out << "        int len = restoreLiturgy(s, 'a');\n";
            out << "        if (len != 3 || s[0] != 'b' || s[1] != 'c' || s[2] != 'd') all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) s[i] = (i % 3 == 0) ? 'x' : 'y';\n";
            out << "        int len = restoreLiturgy(s, 'x');\n";
            out << "        int expected = n - (n + 2) / 3;\n";
            out << "        if (len != expected) all_correct = false;\n";
            out << "        for (int i = 0; i < len; i++) if (s[i] != 'y') { all_correct = false; break; }\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 203: // Invert the Verse
            out << "    {\n";
            out << "        string s = \"abcde\";\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s != \"edcba\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"a\";\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s != \"a\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"ab\";\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s != \"ba\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"racecar\";\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s != \"racecar\") all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) s[i] = 'a' + (i % 26);\n";
            out << "        string original = s;\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s[0] != original[n - 1] || s[n - 1] != original[0]) all_correct = false;\n";
            out << "        // Reverse again — should restore original\n";
            out << "        restoreLiturgy(s, 0);\n";
            out << "        if (s != original) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 204: // Compress the Litany
            out << "    {\n";
            out << "        string s = \"aaabbc\";\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        if (len != 6 || s.substr(0, 6) != \"a3b2c1\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"abc\";\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        if (len != 6 || s.substr(0, 6) != \"a1b1c1\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"aaa\";\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        if (len != 2 || s.substr(0, 2) != \"a3\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"a\";\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        if (len != 2 || s.substr(0, 2) != \"a1\") all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string s = \"aabbcc\";\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        if (len != 6 || s.substr(0, 6) != \"a2b2c2\") all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        // Build string with runs of 5: \"aaaaabbbbbccccc...\"\n";
            out << "        string s;\n";
            out << "        s.reserve(n * 2);\n";
            out << "        for (int i = 0; i < n / 5; i++)\n";
            out << "            for (int j = 0; j < 5; j++)\n";
            out << "                s += ('a' + (i % 26));\n";
            out << "        int distinct = n / 5;\n";
            out << "        int len = restoreLiturgy(s, 0);\n";
            out << "        // Each run becomes char + '5' = 2 chars\n";
            out << "        if (len != distinct * 2) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 205: // Verify the Canon
            out << "    {\n";
            out << "        string a = \"listen\", b = \"silent\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"hello\", b = \"world\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"aab\", b = \"aba\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"ab\", b = \"abc\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"a\", b = \"a\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"\", b = \"\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string a(n, ' '), b(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) { a[i] = 'a' + (i % 26); b[i] = 'a' + ((n - 1 - i) % 26); }\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "        b[0] = 'A';\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 206: // The Cyclic Prayer (boss)
            out << "    {\n";
            out << "        string a = \"abcde\", b = \"cdeab\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"abcde\", b = \"abced\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"a\", b = \"a\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"ab\", b = \"ba\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"abc\", b = \"ab\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"aaa\", b = \"aaa\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        string a = \"abcabc\", b = \"cabcab\";\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string a(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) a[i] = 'a' + (i % 26);\n";
            out << "        // Create a rotation: shift by n/3\n";
            out << "        string b = a.substr(n / 3) + a.substr(0, n / 3);\n";
            out << "        if (restoreLiturgy(a, b, 0) != 1) all_correct = false;\n";
            out << "        // Break it\n";
            out << "        b[0] = 'Z';\n";
            out << "        if (restoreLiturgy(a, b, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
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

    out << "const { restoreLiturgy } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 201: // Scan the Corruption
            out << "{\n";
            out << "    let s = \"aababc\";\n";
            out << "    if (restoreLiturgy(s, 'a'.charCodeAt(0)) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"hello\";\n";
            out << "    if (restoreLiturgy(s, 'z'.charCodeAt(0)) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"xxx\";\n";
            out << "    if (restoreLiturgy(s, 'x'.charCodeAt(0)) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = \"a\";\n";
            out << "    if (restoreLiturgy(s, 'a'.charCodeAt(0)) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = '';\n";
            out << "    for (let i = 0; i < n; i++) s += String.fromCharCode(97 + (i % 26));\n";
            out << "    let expected = Math.ceil(n / 26);\n";
            out << "    if (restoreLiturgy(s, 'a'.charCodeAt(0)) !== expected) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 10; }\n";
            break;

        case 202: // Purge the Glyph
            out << "{\n";
            out << "    let s = { value: \"aababc\" };\n";
            out << "    let len = restoreLiturgy(s, 'a'.charCodeAt(0));\n";
            out << "    if (len !== 3 || s.value[0] !== 'b' || s.value[1] !== 'b' || s.value[2] !== 'c') allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"xxx\" };\n";
            out << "    let len = restoreLiturgy(s, 'x'.charCodeAt(0));\n";
            out << "    if (len !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"abc\" };\n";
            out << "    let len = restoreLiturgy(s, 'z'.charCodeAt(0));\n";
            out << "    if (len !== 3 || s.value.substring(0, 3) !== \"abc\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"abacada\" };\n";
            out << "    let len = restoreLiturgy(s, 'a'.charCodeAt(0));\n";
            out << "    if (len !== 3 || s.value[0] !== 'b' || s.value[1] !== 'c' || s.value[2] !== 'd') allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = { value: '' };\n";
            out << "    for (let i = 0; i < n; i++) s.value += (i % 3 === 0) ? 'x' : 'y';\n";
            out << "    let len = restoreLiturgy(s, 'x'.charCodeAt(0));\n";
            out << "    let expected = n - Math.floor((n + 2) / 3);\n";
            out << "    if (len !== expected) allCorrect = false;\n";
            out << "    for (let i = 0; i < len; i++) if (s.value[i] !== 'y') { allCorrect = false; break; }\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 203: // Invert the Verse
            out << "{\n";
            out << "    let s = { value: \"abcde\" };\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value !== \"edcba\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"a\" };\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value !== \"a\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"ab\" };\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value !== \"ba\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"racecar\" };\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value !== \"racecar\") allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = { value: '' };\n";
            out << "    for (let i = 0; i < n; i++) s.value += String.fromCharCode(97 + (i % 26));\n";
            out << "    let original = s.value;\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value[0] !== original[n - 1] || s.value[n - 1] !== original[0]) allCorrect = false;\n";
            out << "    restoreLiturgy(s, 0);\n";
            out << "    if (s.value !== original) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 204: // Compress the Litany
            out << "{\n";
            out << "    let s = { value: \"aaabbc\" };\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== 6 || s.value.substring(0, 6) !== \"a3b2c1\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"abc\" };\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== 6 || s.value.substring(0, 6) !== \"a1b1c1\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"aaa\" };\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== 2 || s.value.substring(0, 2) !== \"a3\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"a\" };\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== 2 || s.value.substring(0, 2) !== \"a1\") allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = { value: \"aabbcc\" };\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== 6 || s.value.substring(0, 6) !== \"a2b2c2\") allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = { value: '' };\n";
            out << "    for (let i = 0; i < Math.floor(n / 5); i++)\n";
            out << "        for (let j = 0; j < 5; j++)\n";
            out << "            s.value += String.fromCharCode(97 + (i % 26));\n";
            out << "    let distinct = Math.floor(n / 5);\n";
            out << "    let len = restoreLiturgy(s, 0);\n";
            out << "    if (len !== distinct * 2) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;

        case 205: // Verify the Canon
            out << "{\n";
            out << "    let a = \"listen\", b = \"silent\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"hello\", b = \"world\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"aab\", b = \"aba\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"ab\", b = \"abc\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"a\", b = \"a\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"\", b = \"\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = '', b = '';\n";
            out << "    for (let i = 0; i < n; i++) { a += String.fromCharCode(97 + (i % 26)); b += String.fromCharCode(97 + ((n - 1 - i) % 26)); }\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "    b = 'A' + b.substring(1);\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 206: // The Cyclic Prayer (boss)
            out << "{\n";
            out << "    let a = \"abcde\", b = \"cdeab\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"abcde\", b = \"abced\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"a\", b = \"a\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"ab\", b = \"ba\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"abc\", b = \"ab\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"aaa\", b = \"aaa\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = \"abcabc\", b = \"cabcab\";\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = '';\n";
            out << "    for (let i = 0; i < n; i++) a += String.fromCharCode(97 + (i % 26));\n";
            out << "    let split = Math.floor(n / 3);\n";
            out << "    let b = a.substring(split) + a.substring(0, split);\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 1) allCorrect = false;\n";
            out << "    b = 'Z' + b.substring(1);\n";
            out << "    if (restoreLiturgy(a, b, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 16; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
    }
}
