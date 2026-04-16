#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> card_catalog::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        2001, "Count by Prefix", WORLD_NAME, WORLD_DESC,
        "Count how many library words (titles[1..]) start with the prefix titles[0]. target is unused.",
        "Example: [\"app\",\"apple\",\"application\",\"apt\",\"bat\"] -> 2 (apple, application)\nExample: [\"b\",\"apple\",\"bat\",\"ball\"] -> 2\nExample: [\"xyz\",\"abc\"] -> 0\nExample: [\"\",\"abc\",\"def\"] -> 2 (empty prefix matches all)",
        "1 <= titles.size() <= 10,001\ntarget is unused.",
        10000, 1000, 300,
        "int searchCatalog(vector<string>& titles, int target)",
        "Build a trie from library words. Insert each word character by character, incrementing a prefix count at each node. To query, walk the trie along the prefix and return the count at the last node. O(total characters).", generateStub, generateRunner
    });

    waves.push_back({
        2002, "Exact Match", WORLD_NAME, WORLD_DESC,
        "Return 1 if titles[0] is an exact word in the library (titles[1..]), 0 if it is only a prefix or absent. target is unused.",
        "Example: [\"apple\",\"apple\",\"app\",\"application\"] -> 1\nExample: [\"app\",\"apple\",\"application\"] -> 0\nExample: [\"xyz\",\"abc\"] -> 0\nExample: [\"a\",\"a\"] -> 1",
        "1 <= titles.size() <= 10,001\ntarget is unused.",
        10000, 1000, 300,
        "int searchCatalog(vector<string>& titles, int target)",
        "Same trie, but mark end-of-word nodes during insertion. Walk the prefix -- if you reach the end and the node is marked as a word, return 1. O(word length).", generateStub, generateRunner
    });

    waves.push_back({
        2003, "Longest Common Prefix", WORLD_NAME, WORLD_DESC,
        "Return the length of the longest common prefix among ALL words in titles (titles[0..] are all words, no separate query). target is unused.",
        "Example: [\"flower\",\"flow\",\"flight\"] -> 2 (\"fl\")\nExample: [\"dog\",\"racecar\",\"car\"] -> 0\nExample: [\"abc\",\"abc\",\"abc\"] -> 3\nExample: [\"a\"] -> 1",
        "1 <= titles.size() <= 10,000\ntarget is unused.",
        10000, 1000, 300,
        "int searchCatalog(vector<string>& titles, int target)",
        "Build a trie from all words. Walk from root as long as each node has exactly one child and is not an end-of-word. The depth you reach is the LCP length. O(total characters). Alternative: sort and compare first/last string.", generateStub, generateRunner
    });

    waves.push_back({
        2004, "Count the Cards", WORLD_NAME, WORLD_DESC,
        "Count total distinct prefixes across all library words (titles[0..], no query). Equivalent to the number of trie nodes excluding the root. target is unused.",
        "Example: [\"abc\",\"abd\",\"b\"] -> 5 (a, ab, abc, abd, b)\nExample: [\"a\",\"a\"] -> 1 (just \"a\")\nExample: [\"abc\"] -> 3",
        "1 <= titles.size() <= 1,000\nWord length <= 10\ntarget is unused.",
        1000, 1000, 300,
        "int searchCatalog(vector<string>& titles, int target)",
        "Build a trie. Count the total number of nodes created (excluding root). Each node represents a distinct prefix. O(total characters).", generateStub, generateRunner
    });

    waves.push_back({
        2005, "The Index (boss)", WORLD_NAME, WORLD_DESC,
        "Word break: can titles[0] be segmented into words from titles[1..]? Return 1 if yes, 0 otherwise. target is unused.",
        "Example: [\"leetcode\",\"leet\",\"code\"] -> 1\nExample: [\"applepenapple\",\"apple\",\"pen\"] -> 1\nExample: [\"catsandog\",\"cats\",\"dog\",\"sand\",\"and\",\"cat\"] -> 0\nExample: [\"a\",\"a\"] -> 1",
        "1 <= titles[0].length() <= 300\n1 <= dictionary size <= 1,000\ntarget is unused.",
        1000, 1000, 300,
        "int searchCatalog(vector<string>& titles, int target)",
        "Build a trie from dictionary words. Use DP: dp[i] = true if s[0..i-1] can be segmented. For each position, try all trie matches starting at i. If any leads to dp[j]=true, set dp[i]=true. O(n * max_word_length).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a trie-based approach",
        "Need prefix-based branching over characters for fast lookup and prefix aggregation",
        "O(total chars) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Trie plus dynamic programming over string positions",
            "Sliding window with counts",
            "Union-Find over adjacent characters",
            "Binary search over split points"
        },
        0,
        "Need to reuse overlapping subproblems instead of recomputing them",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(total chars) space")
    );
    waves[0].clear_prompt = "Given `titles` where `titles[0]` is a prefix query and the remaining entries are dictionary words, return how many dictionary words start with that prefix.";
    waves[0].flavor_text = "The drowned archive's last librarian runs a fingertip along swollen card edges and whispers the beginning of a title into the damp air. She needs to know how many surviving entries still answer to that opening sound.";
    waves[1].clear_prompt = "Given `titles` where `titles[0]` is a query word and the remaining entries are dictionary words, return `1` if the query is an exact word in the dictionary and `0` otherwise.";
    waves[1].flavor_text = "Some names in the catalog are complete and sanctioned; others are only fragments leading deeper into the shelves. The librarian must decide whether the queried title is truly present or only the beginning of something longer.";
    waves[2].clear_prompt = "Given an array of words `titles`, return the length of their longest common prefix.";
    waves[2].flavor_text = "Dust falls from the upper stacks as the librarian compares a shelf of titles letter by letter. For a time they travel together, then one character betrays the split. The shared beginning before that fracture is the answer she needs.";
    waves[3].clear_prompt = "Given an array of words `titles`, return the number of distinct prefixes across all words.";
    waves[3].flavor_text = "The card catalog is a forest of beginnings. The librarian counts not the books themselves, but every distinct prefix-path a reader could follow before a title branches away into its own corridor.";
    waves[4].clear_prompt = "Given `titles` where `titles[0]` is a target string and the remaining entries form a dictionary, return `1` if the target can be segmented into dictionary words and `0` otherwise.";
    waves[4].flavor_text = "A final index card has been torn into one long unbroken strip of letters. The librarian must decide whether it can still be read as real words from the archive, or whether the message is only convincing noise left by the flood.";
    return waves;
}

void card_catalog::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
  if (lang == Language::CPP) {
    std::string path = player_dir + "/runner.cpp";
    std::ofstream out(path);

    out << "#include <vector>\n";
    out << "#include <string>\n";
    out << "#include <chrono>\n";
    out << "#include <cstdio>\n";
    out << "#include <cstdlib>\n";
    out << "#include <algorithm>\n";
    out << "#include <unordered_map>\n";
    out << "using namespace std;\n\n";

    out << "int searchCatalog(vector<string>& titles, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 2001:
            out << "    {\n";
            out << "        vector<string> t = {\"app\",\"apple\",\"application\",\"apt\",\"bat\"};\n";
            out << "        if (searchCatalog(t, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"b\",\"apple\",\"bat\",\"ball\"};\n";
            out << "        if (searchCatalog(t, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"xyz\",\"abc\"};\n";
            out << "        if (searchCatalog(t, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"\",\"abc\",\"def\"};\n";
            out << "        if (searchCatalog(t, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> t;\n";
            out << "        t.push_back(\"prefix\");\n";
            out << "        for (int i = 0; i < n; i++) t.push_back(\"prefix\" + to_string(i));\n";
            out << "        if (searchCatalog(t, 0) != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2002:
            out << "    {\n";
            out << "        vector<string> t = {\"apple\",\"apple\",\"app\",\"application\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"app\",\"apple\",\"application\"};\n";
            out << "        if (searchCatalog(t, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"xyz\",\"abc\"};\n";
            out << "        if (searchCatalog(t, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"a\",\"a\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> t;\n";
            out << "        t.push_back(\"word5000\");\n";
            out << "        for (int i = 0; i < n; i++) t.push_back(\"word\" + to_string(i));\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 2003:
            out << "    {\n";
            out << "        vector<string> t = {\"flower\",\"flow\",\"flight\"};\n";
            out << "        if (searchCatalog(t, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"dog\",\"racecar\",\"car\"};\n";
            out << "        if (searchCatalog(t, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"abc\",\"abc\",\"abc\"};\n";
            out << "        if (searchCatalog(t, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"a\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string word(100, 'a');\n";
            out << "        vector<string> t(n, word);\n";
            out << "        if (searchCatalog(t, 0) != 100) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 11; }\n";
            break;

        case 2004:
            out << "    {\n";
            out << "        vector<string> t = {\"abc\",\"abd\",\"b\"};\n";
            out << "        if (searchCatalog(t, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"a\",\"a\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"abc\"};\n";
            out << "        if (searchCatalog(t, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> t;\n";
            out << "        for (int i = 0; i < n; i++) t.push_back(\"abcdefghij\");\n";
            out << "        // All identical 10-char strings -> 10 trie nodes\n";
            out << "        if (searchCatalog(t, 0) != 10) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 8; }\n";
            break;

        case 2005:
            out << "    {\n";
            out << "        vector<string> t = {\"leetcode\",\"leet\",\"code\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"applepenapple\",\"apple\",\"pen\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"catsandog\",\"cats\",\"dog\",\"sand\",\"and\",\"cat\"};\n";
            out << "        if (searchCatalog(t, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> t = {\"a\",\"a\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        // Long string composed of repeated \"ab\" segments with dict {\"ab\"}\n";
            out << "        string s = \"\";\n";
            out << "        for (int i = 0; i < 150; i++) s += \"ab\";\n";
            out << "        vector<string> t = {s, \"ab\"};\n";
            out << "        if (searchCatalog(t, 0) != 1) all_correct = false;\n";
            out << "        // Unsegmentable: \"abcabc\" with dict {\"ab\",\"abc\",\"a\"} but missing \"c\" standalone\n";
            out << "        vector<string> t2 = {\"abcabcx\",\"ab\",\"abc\",\"a\"};\n";
            out << "        if (searchCatalog(t2, 0) != 0) all_correct = false;\n";
            out << "        ops = " << wave.n << ";\n";
            out << "    } else { ops = 12; }\n";
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

    out << "const { searchCatalog } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 2001:
            out << "{\n";
            out << "    let t = ['app','apple','application','apt','bat'];\n";
            out << "    if (searchCatalog(t, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['b','apple','bat','ball'];\n";
            out << "    if (searchCatalog(t, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['xyz','abc'];\n";
            out << "    if (searchCatalog(t, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['','abc','def'];\n";
            out << "    if (searchCatalog(t, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = ['prefix'];\n";
            out << "    for (let i = 0; i < n; i++) t.push('prefix' + i);\n";
            out << "    if (searchCatalog(t, 0) !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 2002:
            out << "{\n";
            out << "    let t = ['apple','apple','app','application'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['app','apple','application'];\n";
            out << "    if (searchCatalog(t, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['xyz','abc'];\n";
            out << "    if (searchCatalog(t, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['a','a'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = ['word5000'];\n";
            out << "    for (let i = 0; i < n; i++) t.push('word' + i);\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 2003:
            out << "{\n";
            out << "    let t = ['flower','flow','flight'];\n";
            out << "    if (searchCatalog(t, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['dog','racecar','car'];\n";
            out << "    if (searchCatalog(t, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['abc','abc','abc'];\n";
            out << "    if (searchCatalog(t, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['a'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let word = 'a'.repeat(100);\n";
            out << "    let t = Array(n).fill(word);\n";
            out << "    if (searchCatalog(t, 0) !== 100) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 11; }\n";
            break;

        case 2004:
            out << "{\n";
            out << "    let t = ['abc','abd','b'];\n";
            out << "    if (searchCatalog(t, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['a','a'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['abc'];\n";
            out << "    if (searchCatalog(t, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let t = [];\n";
            out << "    for (let i = 0; i < n; i++) t.push('abcdefghij');\n";
            out << "    if (searchCatalog(t, 0) !== 10) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 8; }\n";
            break;

        case 2005:
            out << "{\n";
            out << "    let t = ['leetcode','leet','code'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['applepenapple','apple','pen'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['catsandog','cats','dog','sand','and','cat'];\n";
            out << "    if (searchCatalog(t, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let t = ['a','a'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let s = '';\n";
            out << "    for (let i = 0; i < 150; i++) s += 'ab';\n";
            out << "    let t = [s, 'ab'];\n";
            out << "    if (searchCatalog(t, 0) !== 1) allCorrect = false;\n";
            out << "    let t2 = ['abcabcx','ab','abc','a'];\n";
            out << "    if (searchCatalog(t2, 0) !== 0) allCorrect = false;\n";
            out << "    ops = " << wave.n << ";\n";
            out << "} else { ops = 12; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
