#include "wave.h"
#include <fstream>
#include <sstream>

std::vector<WaveDef> loadWaves() {
    std::vector<WaveDef> waves;

    // Wave 1: Find max element — baseline scan, no map needed
    waves.push_back({
        1, "Find Max Element",
        "Given an array of integers, return the maximum element.",
        "Example: [3, 1, 4, 1, 5, 9] -> 9\nExample: [-1, -5, -2] -> -1",
        "1 <= N <= 100\n-10^9 <= nums[i] <= 10^9",
        100, 1000, 300,
        "", "",
        "int findMax(vector<int>& nums)"
    });

    // Wave 2: Contains duplicate — map as existence check
    waves.push_back({
        2, "Contains Duplicate",
        "Given an integer array, return true if any value appears at least twice.",
        "Example: [1, 2, 3, 1] -> true\nExample: [1, 2, 3, 4] -> false",
        "1 <= N <= 10,000\n-10^9 <= nums[i] <= 10^9",
        10000, 1000, 300,
        "", "",
        "bool containsDuplicate(vector<int>& nums)"
    });

    // Wave 3: Most frequent element — map as frequency counter
    waves.push_back({
        3, "Most Frequent Element",
        "Given an integer array, return the element that appears most frequently. If there is a tie, return any of them.",
        "Example: [1, 3, 2, 1, 3, 1] -> 1\nExample: [5, 5, 4, 4, 4] -> 4",
        "1 <= N <= 10,000\n-10^9 <= nums[i] <= 10^9\nA unique most frequent element is guaranteed.",
        10000, 1000, 300,
        "", "",
        "int mostFrequent(vector<int>& nums)"
    });

    // Wave 4: Valid anagram — map as consumable resource (build up, tear down)
    waves.push_back({
        4, "Valid Anagram",
        "Given two strings s and t, return true if t is an anagram of s (same characters, same counts, rearranged).",
        "Example: \"anagram\", \"nagaram\" -> true\nExample: \"rat\", \"car\" -> false",
        "1 <= s.length, t.length <= 10,000\nStrings contain lowercase English letters.",
        10000, 1000, 300,
        "", "",
        "bool isAnagram(string s, string t)"
    });

    // Wave 5: Two Sum — complement lookup
    waves.push_back({
        5, "Two Sum",
        "Given an array of integers and a target, return indices of the two numbers that add up to target. Each input has exactly one solution.",
        "Example: [2, 7, 11, 15], target=9 -> [0, 1]\nExample: [3, 2, 4], target=6 -> [1, 2]",
        "2 <= N <= 1,000\n-10^9 <= nums[i] <= 10^9\nExactly one solution exists.",
        1000, 1000, 300,
        "", "",
        "vector<int> twoSum(vector<int>& nums, int target)"
    });

    // Wave 6: Two Sum (large N) — scale pressure, prove O(n)
    waves.push_back({
        6, "Two Sum (Large N)",
        "Same as before, but N is 1,000,000. Your solution must be O(n).",
        "Example: [2, 7, 11, 15], target=9 -> [0, 1]",
        "2 <= N <= 1,000,000\n-10^9 <= nums[i] <= 10^9\nExactly one solution exists.\nMust run in O(n) time.",
        1000000, 200, 300,
        "", "",
        "vector<int> twoSum(vector<int>& nums, int target)"
    });

    // Wave 7: Isomorphic strings — bidirectional mapping, map encodes a relationship
    waves.push_back({
        7, "Isomorphic Strings",
        "Given two strings s and t, determine if they are isomorphic. Two strings are isomorphic if each character in s can be replaced to get t, with a consistent one-to-one mapping. No two characters may map to the same character.",
        "Example: \"egg\", \"add\" -> true\nExample: \"foo\", \"bar\" -> false\nExample: \"paper\", \"title\" -> true",
        "1 <= s.length == t.length <= 10,000\nStrings contain ASCII characters.",
        10000, 1000, 300,
        "", "",
        "bool isIsomorphic(string s, string t)"
    });

    // Wave 8: Group anagrams — computed/canonical keys
    waves.push_back({
        8, "Group Anagrams",
        "Given an array of strings, group the anagrams together. Return a list of groups (order does not matter).",
        "Example: [\"eat\",\"tea\",\"tan\",\"ate\",\"nat\",\"bat\"] -> [[\"eat\",\"tea\",\"ate\"],[\"tan\",\"nat\"],[\"bat\"]]",
        "1 <= N <= 1,000\nStrings contain lowercase English letters.\n0 <= strs[i].length <= 100",
        1000, 1000, 300,
        "", "",
        "vector<vector<string>> groupAnagrams(vector<string>& strs)"
    });

    // Wave 9: Top K frequent elements — two maps, freq + bucket inversion
    waves.push_back({
        9, "Top K Frequent Elements",
        "Given an integer array and an integer k, return the k most frequent elements. You may return the answer in any order.",
        "Example: [1,1,1,2,2,3], k=2 -> [1,2]\nExample: [1], k=1 -> [1]",
        "1 <= N <= 100,000\n1 <= k <= number of distinct elements.\nAnswer is guaranteed to be unique.",
        100000, 1000, 300,
        "", "",
        "vector<int> topKFrequent(vector<int>& nums, int k)"
    });

    // Wave 10: Subarray sum equals K — map + prefix sum, world boss
    waves.push_back({
        10, "Subarray Sum Equals K",
        "Given an array of integers and an integer k, return the total number of contiguous subarrays whose sum equals k.",
        "Example: [1,1,1], k=2 -> 2\nExample: [1,2,3], k=3 -> 2\nExample: [1,-1,0], k=0 -> 3",
        "1 <= N <= 1,000,000\n-1000 <= nums[i] <= 1000\n-10^7 <= k <= 10^7\nMust run in O(n) time.",
        1000000, 500, 300,
        "", "",
        "int subarraySum(vector<int>& nums, int k)"
    });

    return waves;
}

void generateSolutionStub(const WaveDef& wave, const std::string& player_dir) {
    std::string path = player_dir + "/solution.cpp";
    std::ofstream out(path);

    out << "#include <vector>\n";
    out << "#include <unordered_map>\n";
    out << "#include <unordered_set>\n";
    out << "#include <algorithm>\n";
    out << "#include <string>\n";
    out << "using namespace std;\n\n";
    out << "// Wave " << wave.id << ": " << wave.name << "\n";
    out << "// " << wave.description << "\n";
    out << "//\n";
    out << "// " << wave.constraints << "\n";
    out << "//\n";
    out << "// Signature: " << wave.func_signature << "\n\n";

    switch (wave.id) {
        case 1:
            out << "int findMax(vector<int>& nums) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            break;
        case 2:
            out << "bool containsDuplicate(vector<int>& nums) {\n";
            out << "    // TODO: implement\n";
            out << "    return false;\n";
            out << "}\n";
            break;
        case 3:
            out << "int mostFrequent(vector<int>& nums) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            break;
        case 4:
            out << "bool isAnagram(string s, string t) {\n";
            out << "    // TODO: implement\n";
            out << "    return false;\n";
            out << "}\n";
            break;
        case 5:
        case 6:
            out << "vector<int> twoSum(vector<int>& nums, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return {};\n";
            out << "}\n";
            break;
        case 7:
            out << "bool isIsomorphic(string s, string t) {\n";
            out << "    // TODO: implement\n";
            out << "    return false;\n";
            out << "}\n";
            break;
        case 8:
            out << "vector<vector<string>> groupAnagrams(vector<string>& strs) {\n";
            out << "    // TODO: implement\n";
            out << "    return {};\n";
            out << "}\n";
            break;
        case 9:
            out << "vector<int> topKFrequent(vector<int>& nums, int k) {\n";
            out << "    // TODO: implement\n";
            out << "    return {};\n";
            out << "}\n";
            break;
        case 10:
            out << "int subarraySum(vector<int>& nums, int k) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            break;
    }

    out.close();
}

void generateRunner(const WaveDef& wave, const std::string& player_dir) {
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

    // Forward declarations
    switch (wave.id) {
        case 1:
            out << "int findMax(vector<int>& nums);\n\n";
            break;
        case 2:
            out << "bool containsDuplicate(vector<int>& nums);\n\n";
            break;
        case 3:
            out << "int mostFrequent(vector<int>& nums);\n\n";
            break;
        case 4:
            out << "bool isAnagram(string s, string t);\n\n";
            break;
        case 5:
        case 6:
            out << "vector<int> twoSum(vector<int>& nums, int target);\n\n";
            break;
        case 7:
            out << "bool isIsomorphic(string s, string t);\n\n";
            break;
        case 8:
            out << "vector<vector<string>> groupAnagrams(vector<string>& strs);\n\n";
            break;
        case 9:
            out << "vector<int> topKFrequent(vector<int>& nums, int k);\n\n";
            break;
        case 10:
            out << "int subarraySum(vector<int>& nums, int k);\n\n";
            break;
    }

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1: // Find max
            out << "    {\n";
            out << "        vector<int> v = {3, 1, 4, 1, 5, 9, 2, 6};\n";
            out << "        if (findMax(v) != 9) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {-1, -5, -2, -8};\n";
            out << "        if (findMax(v) != -1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {42};\n";
            out << "        if (findMax(v) != 42) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> v(100);\n";
            out << "        for (int i = 0; i < 100; i++) v[i] = i - 50;\n";
            out << "        v[37] = 99999;\n";
            out << "        if (findMax(v) != 99999) all_correct = false;\n";
            out << "        ops = 100;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 2: // Contains duplicate
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 1};\n";
            out << "        if (containsDuplicate(v) != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3, 4};\n";
            out << "        if (containsDuplicate(v) != false) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        if (containsDuplicate(v) != false) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i;\n";
            out << "        v[n - 1] = 0;\n";
            out << "        if (containsDuplicate(v) != true) all_correct = false;\n";
            out << "        // No duplicates test\n";
            out << "        for (int i = 0; i < n; i++) v[i] = i;\n";
            out << "        if (containsDuplicate(v) != false) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 9; }\n";
            break;

        case 3: // Most frequent element
            out << "    {\n";
            out << "        vector<int> v = {1, 3, 2, 1, 3, 1};\n";
            out << "        if (mostFrequent(v) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {5, 5, 4, 4, 4};\n";
            out << "        if (mostFrequent(v) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {7};\n";
            out << "        if (mostFrequent(v) != 7) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v;\n";
            out << "        for (int i = 0; i < n / 2; i++) v.push_back(i % 100);\n";
            out << "        for (int i = 0; i < n / 2 + 1; i++) v.push_back(999);\n";
            out << "        if (mostFrequent(v) != 999) all_correct = false;\n";
            out << "        ops = (int)v.size();\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 4: // Valid anagram
            out << "    {\n";
            out << "        if (isAnagram(\"anagram\", \"nagaram\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isAnagram(\"rat\", \"car\") != false) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isAnagram(\"a\", \"a\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isAnagram(\"ab\", \"ba\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isAnagram(\"aacc\", \"ccac\") != false) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s(n, 'a'), t(n, 'a');\n";
            out << "        for (int i = 0; i < n; i++) { s[i] = 'a' + (i % 26); t[i] = 'a' + ((n - 1 - i) % 26); }\n";
            out << "        if (isAnagram(s, t) != true) all_correct = false;\n";
            out << "        t[0] = 'z'; t[1] = 'z';\n";
            out << "        if (isAnagram(s, t) != false) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 20; }\n";
            break;

        case 5: // Two Sum (small)
            out << "    {\n";
            out << "        vector<int> v = {2, 7, 11, 15};\n";
            out << "        auto r = twoSum(v, 9);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 0 || r[1] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {3, 2, 4};\n";
            out << "        auto r = twoSum(v, 6);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 1 || r[1] != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {3, 3};\n";
            out << "        auto r = twoSum(v, 6);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 0 || r[1] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        // All negative except two large positives — guarantees unique pair\n";
            out << "        for (int i = 0; i < n; i++) v[i] = -(i + 1);\n";
            out << "        v[200] = 500000;\n";
            out << "        v[800] = 500001;\n";
            out << "        int target = 1000001;\n";
            out << "        auto r = twoSum(v, target);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 200 || r[1] != 800) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 11; }\n";
            break;

        case 6: // Two Sum (large N)
            out << "    {\n";
            out << "        vector<int> v = {2, 7, 11, 15};\n";
            out << "        auto r = twoSum(v, 9);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 0 || r[1] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        // All negative except two large positives — guarantees unique pair\n";
            out << "        for (int i = 0; i < n; i++) v[i] = -(i + 1);\n";
            out << "        v[n/4] = 500000000;\n";
            out << "        v[n/4*3] = 500000001;\n";
            out << "        int target = 1000000001;\n";
            out << "        auto r = twoSum(v, target);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != n/4 || r[1] != n/4*3) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 4; }\n";
            break;

        case 7: // Isomorphic strings
            out << "    {\n";
            out << "        if (isIsomorphic(\"egg\", \"add\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isIsomorphic(\"foo\", \"bar\") != false) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isIsomorphic(\"paper\", \"title\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isIsomorphic(\"ab\", \"aa\") != false) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        if (isIsomorphic(\"a\", \"a\") != true) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        string s(n, ' '), t(n, ' ');\n";
            out << "        for (int i = 0; i < n; i++) { s[i] = 'a' + (i % 26); t[i] = 'A' + (i % 26); }\n";
            out << "        if (isIsomorphic(s, t) != true) all_correct = false;\n";
            out << "        // Break the mapping\n";
            out << "        t[n - 1] = t[0];\n";
            out << "        if (isIsomorphic(s, t) != (s[n-1] == s[0])) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 15; }\n";
            break;

        case 8: // Group anagrams
            out << "    {\n";
            out << "        vector<string> v = {\"eat\",\"tea\",\"tan\",\"ate\",\"nat\",\"bat\"};\n";
            out << "        auto groups = groupAnagrams(v);\n";
            out << "        // Sort each group and sort the groups for comparison\n";
            out << "        for (auto& g : groups) sort(g.begin(), g.end());\n";
            out << "        sort(groups.begin(), groups.end());\n";
            out << "        if (groups.size() != 3) all_correct = false;\n";
            out << "        else {\n";
            out << "            vector<vector<string>> expected = {{\"ate\",\"eat\",\"tea\"},{\"bat\"},{\"nat\",\"tan\"}};\n";
            out << "            sort(expected.begin(), expected.end());\n";
            out << "            if (groups != expected) all_correct = false;\n";
            out << "        }\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<string> v = {\"\"};\n";
            out << "        auto groups = groupAnagrams(v);\n";
            out << "        if (groups.size() != 1 || groups[0].size() != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<string> v;\n";
            out << "        for (int i = 0; i < n; i++) {\n";
            out << "            string s(5, 'a');\n";
            out << "            int x = i % 100;\n";
            out << "            for (int j = 0; j < 5; j++) { s[j] = 'a' + (x % 5); x /= 5; }\n";
            out << "            v.push_back(s);\n";
            out << "        }\n";
            out << "        auto groups = groupAnagrams(v);\n";
            out << "        // Verify total elements preserved\n";
            out << "        int total = 0;\n";
            out << "        for (auto& g : groups) total += (int)g.size();\n";
            out << "        if (total != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 8; }\n";
            break;

        case 9: // Top K frequent elements
            out << "    {\n";
            out << "        vector<int> v = {1,1,1,2,2,3};\n";
            out << "        auto r = topKFrequent(v, 2);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 2 || r[0] != 1 || r[1] != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        auto r = topKFrequent(v, 1);\n";
            out << "        if (r.size() != 1 || r[0] != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {4,4,4,2,2,2,1,1,3};\n";
            out << "        auto r = topKFrequent(v, 3);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 3 || r[0] != 1 || r[1] != 2 || r[2] != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v;\n";
            out << "        // Element 1 appears n/2 times, element 2 appears n/4, element 3 appears n/8, rest once\n";
            out << "        for (int i = 0; i < n/2; i++) v.push_back(1);\n";
            out << "        for (int i = 0; i < n/4; i++) v.push_back(2);\n";
            out << "        for (int i = 0; i < n/8; i++) v.push_back(3);\n";
            out << "        for (int i = 0; i < n - n/2 - n/4 - n/8; i++) v.push_back(1000 + i);\n";
            out << "        auto r = topKFrequent(v, 3);\n";
            out << "        sort(r.begin(), r.end());\n";
            out << "        if (r.size() != 3 || r[0] != 1 || r[1] != 2 || r[2] != 3) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 16; }\n";
            break;

        case 10: // Subarray sum equals K
            out << "    {\n";
            out << "        vector<int> v = {1, 1, 1};\n";
            out << "        if (subarraySum(v, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, 2, 3};\n";
            out << "        if (subarraySum(v, 3) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1, -1, 0};\n";
            out << "        if (subarraySum(v, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        if (subarraySum(v, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> v = {1};\n";
            out << "        if (subarraySum(v, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<int> v(n);\n";
            out << "        // Alternating 1, -1 so prefix sums oscillate; k=0 gives many subarrays\n";
            out << "        for (int i = 0; i < n; i++) v[i] = (i % 2 == 0) ? 1 : -1;\n";
            out << "        int result = subarraySum(v, 0);\n";
            out << "        // Every pair of adjacent elements sums to 0, and every even-length subarray from even index\n";
            out << "        if (result < n/2) all_correct = false; // sanity check: at least n/2 such subarrays\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 9; }\n";
            break;
    }

    out << "\n    auto end = chrono::high_resolution_clock::now();\n";
    out << "    int ms = (int)chrono::duration_cast<chrono::milliseconds>(end - start).count();\n\n";
    out << "    printf(\"%d %d\\n\", ms, ops);\n";
    out << "    return all_correct ? 0 : 1;\n";
    out << "}\n";

    out.close();
}
