#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

static WaveQuiz makeHashMapQuiz(const std::string& complexity) {
    WaveQuiz quiz;
    quiz.mode = QuizMode::THEMED_LIGHT;
    quiz.questions.push_back({
        "Why does a hash-map-based approach fit this wave best?",
        {
            "Need fast lookup of prior values or counts",
            "Need to maintain a valid contiguous window",
            "Need monotonic search over the answer space",
            "Need repeated best-next selection"
        },
        0
    });
    quiz.questions.push_back({
        "What is the expected complexity of the intended solution?",
        {
            "O(n) time / O(1) space",
            complexity,
            "O(n log n) time / O(1) space",
            "O(n^2) time / O(n) space"
        },
        1
    });
    return quiz;
}

std::vector<WaveDef> hash_maps::loadWaves() {
    std::vector<WaveDef> waves;

    WaveDef wave1 = {
        1, "Catalog the Stash", WORLD_NAME, WORLD_DESC,
        "Count how many strains appear more than target times in the stash.",
        "Example: [\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"], target=2 -> 1 (only \"kush\" has count > 2)\nExample: [\"a\",\"b\",\"c\"], target=0 -> 3",
        "1 <= N <= 100,000",
        100000, 1000, 300,
        "int tendTheHash(vector<string>& stash, int target)",
        "Build an unordered_map from string to count by scanning the stash once. Then iterate the map values and count how many exceed target. O(n) time, O(k) space where k is the number of distinct strains.", generateStub, generateRunner
    };
    wave1.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave1);

    WaveDef wave2 = {
        2, "Peak Harvest", WORLD_NAME, WORLD_DESC,
        "Return the highest frequency of any single strain in the stash.",
        "Example: [\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"] -> 3\nExample: [\"a\",\"b\",\"c\"] -> 1",
        "1 <= N <= 100,000\ntarget is unused.",
        100000, 1000, 300,
        "int tendTheHash(vector<string>& stash, int target)",
        "Build a frequency map, then iterate its values tracking the maximum count seen. O(n) time, O(k) space. A single pass over the map entries after building it is enough to find the peak.", generateStub, generateRunner
    };
    wave2.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave2);

    WaveDef wave3 = {
        3, "The Blend", WORLD_NAME, WORLD_DESC,
        "Return 1 if two different strains have counts that sum to exactly target. Return 0 otherwise.",
        "Example: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], target=5 -> 1 (a=3, b=2, 3+2=5)\nExample: [\"a\",\"a\",\"b\",\"b\"], target=5 -> 0",
        "1 <= N <= 200,000\nStrains in the pair must be different.",
        200000, 1000, 300,
        "int tendTheHash(vector<string>& stash, int target)",
        "Build a frequency map, then apply the two-sum pattern on the map values. For each count, check if (target - count) exists in a seen set of previous counts, being careful that the two strains must be different. O(n) time for building the map, O(k) for the two-sum check.", generateStub, generateRunner
    };
    wave3.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave3);

    WaveDef wave4 = {
        4, "Compare Harvests", WORLD_NAME, WORLD_DESC,
        "Return 1 if stash and batch2 have identical frequency profiles (same strains, same counts). Return 0 otherwise. Parameter batch2 is added this wave.",
        "Example: [\"a\",\"b\",\"a\"], [\"a\",\"a\",\"b\"], target=0 -> 1\nExample: [\"a\",\"b\"], [\"a\",\"c\"], target=0 -> 0",
        "1 <= N, M <= 200,000\ntarget is unused.",
        200000, 1000, 300,
        "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target)",
        "Build a frequency map for each input array, then compare the two maps. They must have the same keys with the same values. O(n+m) time, O(k) space where k is distinct strain count.", generateStub, generateRunner
    };
    wave4.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave4);

    WaveDef wave5 = {
        5, "Rank the Crop", WORLD_NAME, WORLD_DESC,
        "Return the count of the target-th most common strain (1-indexed). target=1 means most common.",
        "Example: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], target=1 -> 3 (a)\nExample: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], target=2 -> 2 (b)\nExample: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], target=3 -> 1 (c)",
        "1 <= N <= 500,000\n1 <= target <= number of distinct strains.\nbatch2 is empty.",
        500000, 1000, 300,
        "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target)",
        "Build a frequency map, collect the count values into a vector, sort that vector descending, and return the value at index target-1. O(n log n) due to sorting the counts. The key insight is to reduce the problem to sorting a list of frequencies.", generateStub, generateRunner
    };
    wave5.quiz = makeHashMapQuiz("O(n log n) time / O(k) space");
    waves.push_back(wave5);

    WaveDef wave6 = {
        6, "Smoke Test", WORLD_NAME, WORLD_DESC,
        "Return 1 if no two strains share the same count. Return 0 if any two strains have equal frequency.",
        "Example: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"] -> 1 (counts: 3,2,1 — all unique)\nExample: [\"a\",\"a\",\"b\",\"b\",\"c\"] -> 0 (a and b both have count 2)",
        "1 <= N <= 500,000\ntarget and batch2 unused.",
        500000, 1000, 300,
        "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target)",
        "Build a frequency map, then collect the count values into a set. If the set size equals the map size, every strain has a unique frequency. O(n) time, O(k) space. The key insight is that duplicate detection on counts is just a set-size check.", generateStub, generateRunner
    };
    wave6.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave6);

    WaveDef wave7 = {
        7, "The Audit", WORLD_NAME, WORLD_DESC,
        "Return the minimum number of strains to completely remove from the catalog so that all remaining strains have unique counts.",
        "Example: [\"a\",\"a\",\"b\",\"b\",\"c\"] -> 1 (remove either a or b so counts become unique)\nExample: [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"] -> 0 (already unique: 3,2,1)\nExample: [\"a\",\"a\",\"b\",\"b\",\"c\",\"c\"] -> 2 (three strains share count 2, keep one, remove two)",
        "1 <= N <= 1,000,000\ntarget and batch2 unused.",
        1000000, 500, 300,
        "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target)",
        "Build a frequency map, then build a count-of-counts map (how many strains share each frequency). For each frequency that appears k times, k-1 strains must be removed to make that frequency unique. Sum all (k-1) values. O(n) time, O(k) space.", generateStub, generateRunner
    };
    wave7.quiz = makeHashMapQuiz("O(n) time / O(k) space");
    waves.push_back(wave7);

    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Frequency map plus count-of-counts aggregation",
            "Sliding window over repeated values",
            "Heap of duplicate counts",
            "Binary search on removal count"
        },
        0,
        "Need fast lookup of prior values or counts",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(k) space")
    );
    waves[0].clear_prompt = "Given an array of strings `stash`, return how many distinct strings appear more than `target` times.";
    waves[0].flavor_text = "In Hash Valley, the granary quartermaster walks moonlit rows of labeled sacks, tracking which crop strains have overrun the storehouse. The ones that repeat too often draw rot, thieves, and unwanted attention.";
    waves[1].clear_prompt = "Given an array of strings `stash`, return the largest frequency of any single string.";
    waves[1].flavor_text = "The quartermaster does not care which strain dominates the stash, only how strong the loudest pattern has become. Somewhere in the count is the heaviest repetition, the one that drowns out all the others.";
    waves[2].clear_prompt = "Given an array of strings `stash`, treat each distinct string's frequency as a count. Return `1` if two different strings have frequencies that sum to `target`, otherwise return `0`.";
    waves[2].flavor_text = "Working by touch instead of sight, the quartermaster sorts the harvest into quiet piles. He is hunting for a perfect blend: two distinct strains whose quantities lock into one exact total, the kind of pairing apothecaries whisper about.";
    waves[3].clear_prompt = "Given two arrays of strings, `stash` and `batch2`, return `1` if they have exactly the same frequency map and `0` otherwise.";
    waves[3].flavor_text = "The quartermaster compares two sealed caches in the dark, touching each label only once. If their hidden counts match perfectly, the trade is honest; if not, someone has thinned a barrel and hoped the night would hide it.";
    waves[4].clear_prompt = "Given an array of strings `stash`, return the frequency of the `target`th most common distinct string, where `target` is 1-indexed.";
    waves[4].flavor_text = "The quartermaster ranks the crop by sheer repetition, listening for which labels echo loudest through the inventory. He does not need the whole ordering spoken back, only the exact strength of the entry at the requested rank.";
    waves[5].clear_prompt = "Given an array of strings `stash`, return `1` if every distinct string has a unique frequency and `0` if any two strings share the same count.";
    waves[5].flavor_text = "The ledger is healthy only when every strain carries its own unmistakable weight. The quartermaster taps through the counts, wary of any two entries that throb in perfect unison like a hidden forgery.";
    waves[6].clear_prompt = "Given an array of strings `stash`, return the minimum number of distinct strings that must be removed entirely so that all remaining string frequencies are unique.";
    waves[6].flavor_text = "In the far granary, the quartermaster decides what must be burned to save the rest. Too many strains share the same weight, and sameness invites confusion, mold, and bad trade. He wants the smallest complete cull that leaves every surviving count distinct.";

    return waves;
}

void hash_maps::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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
    out << "#include <unordered_set>\n";
    out << "using namespace std;\n\n";

    // Forward declaration — signature depends on wave
    if (wave.id <= 3) {
        out << "int tendTheHash(vector<string>& stash, int target);\n\n";
    } else {
        out << "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target);\n\n";
    }

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1: // Catalog the Stash
            out << "    {\n";
            out << "        vector<string> s = {\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"};\n";
            out << "        if (tendTheHash(s, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"b\",\"c\"};\n";
            out << "        if (tendTheHash(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\"};\n";
            out << "        if (tendTheHash(s, 3) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\",\"c\",\"c\"};\n";
            out << "        if (tendTheHash(s, 1) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> s;\n";
            out << "        for (int i = 0; i < n; i++) s.push_back(\"strain\" + to_string(i % 100));\n";
            out << "        // Each strain appears n/100 times. target = n/100 - 1 means all 100 qualify\n";
            out << "        if (tendTheHash(s, n/100 - 1) != 100) all_correct = false;\n";
            out << "        // target = n/100 means none qualify (count is not > target, it equals it)\n";
            out << "        if (tendTheHash(s, n/100) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 17; }\n";
            break;

        case 2: // Peak Harvest
            out << "    {\n";
            out << "        vector<string> s = {\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"};\n";
            out << "        if (tendTheHash(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"b\",\"c\"};\n";
            out << "        if (tendTheHash(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"x\"};\n";
            out << "        if (tendTheHash(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"a\",\"a\"};\n";
            out << "        if (tendTheHash(s, 0) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> s;\n";
            out << "        for (int i = 0; i < n; i++) s.push_back(\"strain\" + to_string(i % 50));\n";
            out << "        // Each of 50 strains appears n/50 times\n";
            out << "        if (tendTheHash(s, 0) != n / 50) all_correct = false;\n";
            out << "        // Add extras of one strain to make it dominant\n";
            out << "        for (int i = 0; i < 1000; i++) s.push_back(\"dominant\");\n";
            out << "        if (tendTheHash(s, 0) != 1000) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 3: // The Blend
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"};\n";
            out << "        if (tendTheHash(s, 5) != 1) all_correct = false; // a=3 + b=2 = 5\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\"};\n";
            out << "        if (tendTheHash(s, 5) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\"};\n";
            out << "        if (tendTheHash(s, 4) != 1) all_correct = false; // a=2 + b=2 = 4\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // Must be two DIFFERENT strains\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"a\"};\n";
            out << "        if (tendTheHash(s, 8) != 0) all_correct = false; // can't pair a with itself\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"b\",\"c\"};\n";
            out << "        if (tendTheHash(s, 2) != 1) all_correct = false; // 1+1=2, many pairs\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> s;\n";
            out << "        // 100 strains, strain_i appears i+1 times\n";
            out << "        for (int i = 0; i < 100; i++)\n";
            out << "            for (int j = 0; j <= i; j++)\n";
            out << "                s.push_back(\"strain\" + to_string(i));\n";
            out << "        // strain0=1, strain99=100. target=101 -> 1+100=101, yes\n";
            out << "        if (tendTheHash(s, 101) != 1) all_correct = false;\n";
            out << "        // target=200 -> max pair is 99+100=199, no\n";
            out << "        if (tendTheHash(s, 200) != 0) all_correct = false;\n";
            out << "        ops = (int)s.size();\n";
            out << "    } else { ops = 15; }\n";
            break;

        case 4: // Compare Harvests
            out << "    {\n";
            out << "        vector<string> a = {\"a\",\"b\",\"a\"}, b = {\"a\",\"a\",\"b\"};\n";
            out << "        if (tendTheHash(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> a = {\"a\",\"b\"}, b = {\"a\",\"c\"};\n";
            out << "        if (tendTheHash(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> a = {\"a\",\"a\"}, b = {\"a\"};\n";
            out << "        if (tendTheHash(a, b, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> a = {}, b = {};\n";
            out << "        if (tendTheHash(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> a = {\"x\",\"y\",\"z\"}, b = {\"z\",\"x\",\"y\"};\n";
            out << "        if (tendTheHash(a, b, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> a, b;\n";
            out << "        for (int i = 0; i < n; i++) {\n";
            out << "            a.push_back(\"item\" + to_string(i % 500));\n";
            out << "            b.push_back(\"item\" + to_string(i % 500));\n";
            out << "        }\n";
            out << "        // Same items, same counts — should match\n";
            out << "        if (tendTheHash(a, b, 0) != 1) all_correct = false;\n";
            out << "        // Change one item in b — should not match\n";
            out << "        b[0] = \"DIFFERENT\";\n";
            out << "        if (tendTheHash(a, b, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 13; }\n";
            break;

        case 5: // Rank the Crop
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 1) != 3) all_correct = false; // most common: a=3\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 2) != 2) all_correct = false; // 2nd: b=2\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 3) != 1) all_correct = false; // 3rd: c=1\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"x\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\"}, empty;\n";
            out << "        // Two strains tied at count 2. target=1 and target=2 both return 2.\n";
            out << "        if (tendTheHash(s, empty, 1) != 2) all_correct = false;\n";
            out << "        if (tendTheHash(s, empty, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> s, empty;\n";
            out << "        // strain_i appears (i+1) times: strain0=1, strain1=2, ...\n";
            out << "        int distinct = 0;\n";
            out << "        int total = 0;\n";
            out << "        for (int i = 0; total + i + 1 <= n; i++) {\n";
            out << "            for (int j = 0; j <= i; j++) s.push_back(\"s\" + to_string(i));\n";
            out << "            total += i + 1;\n";
            out << "            distinct = i + 1;\n";
            out << "        }\n";
            out << "        // Most common is the last strain added\n";
            out << "        if (tendTheHash(s, empty, 1) != distinct) all_correct = false;\n";
            out << "        // Least common is strain0 with count 1\n";
            out << "        if (tendTheHash(s, empty, distinct) != 1) all_correct = false;\n";
            out << "        ops = (int)s.size();\n";
            out << "    } else { ops = 18; }\n";
            break;

        case 6: // Smoke Test
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 1) all_correct = false; // counts 3,2,1 — unique\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 0) all_correct = false; // a=2, b=2 — not unique\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"b\",\"c\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 0) all_correct = false; // all count 1 — not unique if >1 strain\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 1) all_correct = false; // a=2, b=1 — unique\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> s, empty;\n";
            out << "        // strain_i appears i+1 times — all unique counts\n";
            out << "        for (int i = 0; i < 200; i++)\n";
            out << "            for (int j = 0; j <= i; j++)\n";
            out << "                s.push_back(\"s\" + to_string(i));\n";
            out << "        if (tendTheHash(s, empty, 0) != 1) all_correct = false;\n";
            out << "        // Add another strain with count 200 (same as s199) — now not unique\n";
            out << "        for (int i = 0; i < 200; i++) s.push_back(\"duplicate\");\n";
            out << "        if (tendTheHash(s, empty, 0) != 0) all_correct = false;\n";
            out << "        ops = (int)s.size();\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 7: // The Audit (boss)
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        // counts: a=2, b=2, c=1. Remove one of a or b -> 1 removal\n";
            out << "        if (tendTheHash(s, empty, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"}, empty;\n";
            out << "        // counts: 3,2,1 — already unique\n";
            out << "        if (tendTheHash(s, empty, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"a\",\"b\",\"b\",\"c\",\"c\"}, empty;\n";
            out << "        // counts: 2,2,2 — keep one, remove two\n";
            out << "        if (tendTheHash(s, empty, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\",\"b\",\"c\",\"d\"}, empty;\n";
            out << "        // counts: 1,1,1,1 — keep one, remove three\n";
            out << "        if (tendTheHash(s, empty, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> s = {\"a\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        // a=3, b=3, c=2, d=2, e=1 -> remove 1 from {a,b} and 1 from {c,d} = 2\n";
            out << "        vector<string> s = {\"a\",\"a\",\"a\",\"b\",\"b\",\"b\",\"c\",\"c\",\"d\",\"d\",\"e\"}, empty;\n";
            out << "        if (tendTheHash(s, empty, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = 1000;\n";
            out << "        vector<string> s, empty;\n";
            out << "        // 10 groups of 100 strains each with the same count\n";
            out << "        // Group i: 100 strains each appearing (i+1) times\n";
            out << "        for (int g = 0; g < 10; g++)\n";
            out << "            for (int i = 0; i < 100; i++)\n";
            out << "                for (int j = 0; j <= g; j++)\n";
            out << "                    s.push_back(\"g\" + to_string(g) + \"_s\" + to_string(i));\n";
            out << "        // Each group has 100 strains with the same count -> need to remove 99 from each group\n";
            out << "        if (tendTheHash(s, empty, 0) != 990) all_correct = false;\n";
            out << "        ops = (int)s.size();\n";
            out << "    } else { ops = 21; }\n";
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

    out << "const { tendTheHash } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1: // Catalog the Stash
            out << "{\n";
            out << "    let s = [\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"];\n";
            out << "    if (tendTheHash(s, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"b\",\"c\"];\n";
            out << "    if (tendTheHash(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\"];\n";
            out << "    if (tendTheHash(s, 3) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\",\"c\",\"c\"];\n";
            out << "    if (tendTheHash(s, 1) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(\"strain\" + String(i % 100));\n";
            out << "    if (tendTheHash(s, Math.floor(n / 100) - 1) !== 100) allCorrect = false;\n";
            out << "    if (tendTheHash(s, Math.floor(n / 100)) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 17; }\n";
            break;

        case 2: // Peak Harvest
            out << "{\n";
            out << "    let s = [\"kush\",\"haze\",\"kush\",\"kush\",\"haze\"];\n";
            out << "    if (tendTheHash(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"b\",\"c\"];\n";
            out << "    if (tendTheHash(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"x\"];\n";
            out << "    if (tendTheHash(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"a\",\"a\"];\n";
            out << "    if (tendTheHash(s, 0) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push(\"strain\" + String(i % 50));\n";
            out << "    if (tendTheHash(s, 0) !== Math.floor(n / 50)) allCorrect = false;\n";
            out << "    for (let i = 0; i < 1000; i++) s.push(\"dominant\");\n";
            out << "    if (tendTheHash(s, 0) !== 1000) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 3: // The Blend
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"];\n";
            out << "    if (tendTheHash(s, 5) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\"];\n";
            out << "    if (tendTheHash(s, 5) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\"];\n";
            out << "    if (tendTheHash(s, 4) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"a\"];\n";
            out << "    if (tendTheHash(s, 8) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"b\",\"c\"];\n";
            out << "    if (tendTheHash(s, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < 100; i++)\n";
            out << "        for (let j = 0; j <= i; j++)\n";
            out << "            s.push(\"strain\" + String(i));\n";
            out << "    if (tendTheHash(s, 101) !== 1) allCorrect = false;\n";
            out << "    if (tendTheHash(s, 200) !== 0) allCorrect = false;\n";
            out << "    ops = s.length;\n";
            out << "} else { ops = 15; }\n";
            break;

        case 4: // Compare Harvests
            out << "{\n";
            out << "    let a = [\"a\",\"b\",\"a\"], b = [\"a\",\"a\",\"b\"];\n";
            out << "    if (tendTheHash(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [\"a\",\"b\"], b = [\"a\",\"c\"];\n";
            out << "    if (tendTheHash(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [\"a\",\"a\"], b = [\"a\"];\n";
            out << "    if (tendTheHash(a, b, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [], b = [];\n";
            out << "    if (tendTheHash(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let a = [\"x\",\"y\",\"z\"], b = [\"z\",\"x\",\"y\"];\n";
            out << "    if (tendTheHash(a, b, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let a = [], b = [];\n";
            out << "    for (let i = 0; i < n; i++) {\n";
            out << "        a.push(\"item\" + String(i % 500));\n";
            out << "        b.push(\"item\" + String(i % 500));\n";
            out << "    }\n";
            out << "    if (tendTheHash(a, b, 0) !== 1) allCorrect = false;\n";
            out << "    b[0] = \"DIFFERENT\";\n";
            out << "    if (tendTheHash(a, b, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 13; }\n";
            break;

        case 5: // Rank the Crop
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 1) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 3) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"x\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 1) !== 2) allCorrect = false;\n";
            out << "    if (tendTheHash(s, empty, 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [], empty = [];\n";
            out << "    let distinct = 0;\n";
            out << "    let total = 0;\n";
            out << "    for (let i = 0; total + i + 1 <= n; i++) {\n";
            out << "        for (let j = 0; j <= i; j++) s.push(\"s\" + String(i));\n";
            out << "        total += i + 1;\n";
            out << "        distinct = i + 1;\n";
            out << "    }\n";
            out << "    if (tendTheHash(s, empty, 1) !== distinct) allCorrect = false;\n";
            out << "    if (tendTheHash(s, empty, distinct) !== 1) allCorrect = false;\n";
            out << "    ops = s.length;\n";
            out << "} else { ops = 18; }\n";
            break;

        case 6: // Smoke Test
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let s = [], empty = [];\n";
            out << "    for (let i = 0; i < 200; i++)\n";
            out << "        for (let j = 0; j <= i; j++)\n";
            out << "            s.push(\"s\" + String(i));\n";
            out << "    if (tendTheHash(s, empty, 0) !== 1) allCorrect = false;\n";
            out << "    for (let i = 0; i < 200; i++) s.push(\"duplicate\");\n";
            out << "    if (tendTheHash(s, empty, 0) !== 0) allCorrect = false;\n";
            out << "    ops = s.length;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 7: // The Audit (boss)
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"b\",\"b\",\"c\",\"c\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"b\",\"c\",\"d\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [\"a\",\"a\",\"a\",\"b\",\"b\",\"b\",\"c\",\"c\",\"d\",\"d\",\"e\"], empty = [];\n";
            out << "    if (tendTheHash(s, empty, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let s = [], empty = [];\n";
            out << "    for (let g = 0; g < 10; g++)\n";
            out << "        for (let i = 0; i < 100; i++)\n";
            out << "            for (let j = 0; j <= g; j++)\n";
            out << "                s.push(\"g\" + String(g) + \"_s\" + String(i));\n";
            out << "    if (tendTheHash(s, empty, 0) !== 990) allCorrect = false;\n";
            out << "    ops = s.length;\n";
            out << "} else { ops = 21; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
    }
}
