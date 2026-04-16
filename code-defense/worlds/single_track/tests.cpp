#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> single_track::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1701, "Merge the Schedule", WORLD_NAME, WORLD_DESC,
        "Merge all overlapping intervals in schedule. Write the merged result back into schedule (resize it). Return the count of merged intervals.",
        "Example: [[1,3],[2,6],[8,10],[15,18]] -> 3 (merged: [1,6],[8,10],[15,18])\nExample: [[1,4],[4,5]] -> 1 ([1,5])\nExample: [[1,2]] -> 1",
        "1 <= N <= 100,000\nIntervals are [start, end] with start <= end.",
        100000, 1000, 300,
        "int dispatch(vector<vector<int>>& schedule, int target)",
        "Sort intervals by start time. Iterate, merging overlapping intervals (current.end >= next.start). O(n log n) for sorting.", generateStub, generateRunner
    });

    waves.push_back({
        1702, "Insert a Train", WORLD_NAME, WORLD_DESC,
        "schedule[0] is a new interval [start, end]. schedule[1..] is a sorted, non-overlapping list of existing intervals. Insert the new interval, merging as needed. Write the result back into schedule. Return the count of resulting intervals.",
        "Example: [[2,5],[1,3],[6,9]] -> 2 (insert [2,5] into [[1,3],[6,9]] -> [[1,5],[6,9]])\nExample: [[4,8],[1,2],[3,5],[6,7],[8,10],[12,16]] -> 3 ([[1,2],[3,10],[12,16]])\nExample: [[1,1],[2,3]] -> 2 ([[1,1],[2,3]])",
        "1 <= N <= 100,000\nExisting intervals are sorted and non-overlapping.",
        100000, 1000, 300,
        "int dispatch(vector<vector<int>>& schedule, int target)",
        "Find where the new interval overlaps (start <= existing.end and end >= existing.start). Merge all overlapping intervals into one. O(n).", generateStub, generateRunner
    });

    waves.push_back({
        1703, "Platform Count", WORLD_NAME, WORLD_DESC,
        "Return the minimum number of platforms needed so that no train waits — i.e. the maximum number of overlapping intervals at any point. target is unused.",
        "Example: [[1,5],[2,6],[4,7]] -> 3\nExample: [[1,2],[3,4],[5,6]] -> 1\nExample: [[1,10],[2,3],[4,5]] -> 2",
        "1 <= N <= 100,000\nIntervals are [start, end] with start <= end.",
        100000, 1000, 300,
        "int dispatch(vector<vector<int>>& schedule, int target)",
        "Separate start and end times into two arrays, sort both. Sweep with two pointers -- increment on start, decrement on end. Track maximum concurrent. O(n log n).", generateStub, generateRunner
    });

    waves.push_back({
        1704, "Can They All Run?", WORLD_NAME, WORLD_DESC,
        "Return 1 if no intervals overlap (a person can attend all meetings), 0 otherwise. Touching endpoints (e.g. [1,2] and [2,3]) do NOT count as overlapping. target is unused.",
        "Example: [[0,30],[5,10],[15,20]] -> 0\nExample: [[7,10],[2,4]] -> 1\nExample: [[1,2],[2,3]] -> 1 (touching is ok)",
        "1 <= N <= 100,000\nIntervals are [start, end] with start <= end.",
        100000, 1000, 300,
        "int dispatch(vector<vector<int>>& schedule, int target)",
        "Sort by start time. Check if any interval starts before the previous one ends. O(n log n).", generateStub, generateRunner
    });

    waves.push_back({
        1705, "The Express", WORLD_NAME, WORLD_DESC,
        "Return the minimum number of intervals to remove so that the remaining intervals do not overlap. Touching endpoints are allowed. target is unused.",
        "Example: [[1,2],[2,3],[3,4],[1,3]] -> 1\nExample: [[1,2],[1,2],[1,2]] -> 2\nExample: [[1,2],[2,3]] -> 0",
        "1 <= N <= 100,000\nIntervals are [start, end] with start <= end.",
        100000, 1000, 300,
        "int dispatch(vector<vector<int>>& schedule, int target)",
        "Sort by end time. Greedily keep the interval that ends earliest. Count intervals that overlap with the kept one as removals. Equivalent to: total - max non-overlapping intervals. O(n log n).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "an interval-sorting approach",
        "Need ranges in sorted order so overlaps and gaps can be processed sequentially",
        "O(1) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Sort intervals and scan greedily",
            "Trie over interval endpoints",
            "Union-Find over overlapping indices",
            "Heap of every pair overlap"
        },
        0,
        "Need ranges in sorted order so overlaps and gaps can be processed sequentially",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(1) space")
    );
    waves[0].clear_prompt = "Given a list of intervals `schedule`, merge all overlapping intervals, write the merged intervals back into `schedule`, and return the new count.";
    waves[0].flavor_text = "Rail timetables lie scattered across a station desk, and the dispatcher must combine journeys that truly overlap into one continuous run of track.";
    waves[1].clear_prompt = "Given `schedule` where the first interval is new and the remaining intervals are sorted and non-overlapping, insert the new interval, merge where needed, and return the resulting count.";
    waves[1].flavor_text = "A fresh train line is being added to a fragile schedule. The dispatcher slides the new interval into place, stitching it to neighbors only where the times truly collide.";
    waves[2].clear_prompt = "Given a list of intervals `schedule`, return the minimum number of platforms required so that no interval waits.";
    waves[2].flavor_text = "The station has more engines than resting places. The dispatcher watches arrivals and departures cross like blades, counting how many platforms must exist if no train is ever left suspended in the fog.";
    waves[3].clear_prompt = "Given a list of intervals `schedule`, return `1` if no intervals overlap under the wave's endpoint rules and `0` otherwise.";
    waves[3].flavor_text = "The single track cannot honor every promise at once. The dispatcher checks the schedule for conflicts, looking for any pair of commitments that try to occupy the same rail.";
    waves[4].clear_prompt = "Given a list of intervals `schedule`, return the minimum number of intervals to remove so the remaining intervals do not overlap.";
    waves[4].flavor_text = "When the express finally approaches, the whole network depends on cutting the fewest possible runs from the timetable. The dispatcher chooses what must be sacrificed so the remaining route stays clean.";
    return waves;
}

void single_track::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int dispatch(vector<vector<int>>& schedule, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1701: // Merge the Schedule
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,3},{2,6},{8,10},{15,18}};\n";
            out << "        if (dispatch(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,4},{4,5}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,4},{2,3}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{3,4},{5,6}};\n";
            out << "        if (dispatch(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> s;\n";
            out << "        for (int i = 0; i < n; i += 2) s.push_back({i, i + 2});\n";
            out << "        // [0,2],[2,4],[4,6]... all touching -> merges into one\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "        ops = n / 2;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 1702: // Insert a Train
            out << "    {\n";
            out << "        vector<vector<int>> s = {{2,5},{1,3},{6,9}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{4,8},{1,2},{3,5},{6,7},{8,10},{12,16}};\n";
            out << "        if (dispatch(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,1},{2,3}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{5,7},{1,3},{4,6},{8,10}};\n";
            out << "        if (dispatch(s, 0) != 3) all_correct = false; // insert [5,7] into [[1,3],[4,6],[8,10]] -> [[1,3],[4,7],[8,10]]\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> s;\n";
            out << "        // New interval that merges many\n";
            out << "        s.push_back({0, n * 3}); // new interval covers everything\n";
            out << "        for (int i = 0; i < n; i++) s.push_back({i * 3, i * 3 + 1});\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 1703: // Platform Count
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,5},{2,6},{4,7}};\n";
            out << "        if (dispatch(s, 0) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{3,4},{5,6}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,10},{2,3},{4,5}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,3},{2,4},{3,5}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> s;\n";
            out << "        // All intervals overlap at point 1: [0,2],[0,2],...\n";
            out << "        for (int i = 0; i < n; i++) s.push_back({0, 2});\n";
            out << "        if (dispatch(s, 0) != n) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 1704: // Can They All Run?
            out << "    {\n";
            out << "        vector<vector<int>> s = {{0,30},{5,10},{15,20}};\n";
            out << "        if (dispatch(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{7,10},{2,4}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{2,3}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false; // touching is ok\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,5},{2,3}};\n";
            out << "        if (dispatch(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{5,8}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> s;\n";
            out << "        // Non-overlapping: [0,1],[1,2],[2,3],...\n";
            out << "        for (int i = 0; i < n; i++) s.push_back({i, i + 1});\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "        ops = n;\n";
            out << "    } else { ops = 10; }\n";
            break;

        case 1705: // The Express (boss)
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{2,3},{3,4},{1,3}};\n";
            out << "        if (dispatch(s, 0) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{1,2},{1,2}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2},{2,3}};\n";
            out << "        if (dispatch(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,100},{11,22},{1,11},{2,12}};\n";
            out << "        if (dispatch(s, 0) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<vector<int>> s = {{1,2}};\n";
            out << "        if (dispatch(s, 0) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        int n = " << wave.n << ";\n";
            out << "        vector<vector<int>> s;\n";
            out << "        // Non-overlapping intervals: nothing to remove\n";
            out << "        for (int i = 0; i < n; i++) s.push_back({i * 2, i * 2 + 1});\n";
            out << "        if (dispatch(s, 0) != 0) all_correct = false;\n";
            out << "        ops = n;\n";
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

    out << "const { dispatch } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1701: // Merge the Schedule
            out << "{\n";
            out << "    let s = [[1,3],[2,6],[8,10],[15,18]];\n";
            out << "    if (dispatch(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,4],[4,5]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,4],[2,3]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2],[3,4],[5,6]];\n";
            out << "    if (dispatch(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i += 2) s.push([i, i + 2]);\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "    ops = Math.floor(n / 2);\n";
            out << "} else { ops = 12; }\n";
            break;

        case 1702: // Insert a Train
            out << "{\n";
            out << "    let s = [[2,5],[1,3],[6,9]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[4,8],[1,2],[3,5],[6,7],[8,10],[12,16]];\n";
            out << "    if (dispatch(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,1],[2,3]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[5,7],[1,3],[4,6],[8,10]];\n";
            out << "    if (dispatch(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [[0, n * 3]];\n";
            out << "    for (let i = 0; i < n; i++) s.push([i * 3, i * 3 + 1]);\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 1703: // Platform Count
            out << "{\n";
            out << "    let s = [[1,5],[2,6],[4,7]];\n";
            out << "    if (dispatch(s, 0) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2],[3,4],[5,6]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,10],[2,3],[4,5]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,3],[2,4],[3,5]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push([0, 2]);\n";
            out << "    if (dispatch(s, 0) !== n) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 1704: // Can They All Run?
            out << "{\n";
            out << "    let s = [[0,30],[5,10],[15,20]];\n";
            out << "    if (dispatch(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[7,10],[2,4]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2],[2,3]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,5],[2,3]];\n";
            out << "    if (dispatch(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[5,8]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push([i, i + 1]);\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "    ops = n;\n";
            out << "} else { ops = 10; }\n";
            break;

        case 1705: // The Express (boss)
            out << "{\n";
            out << "    let s = [[1,2],[2,3],[3,4],[1,3]];\n";
            out << "    if (dispatch(s, 0) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2],[1,2],[1,2]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2],[2,3]];\n";
            out << "    if (dispatch(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,100],[11,22],[1,11],[2,12]];\n";
            out << "    if (dispatch(s, 0) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let s = [[1,2]];\n";
            out << "    if (dispatch(s, 0) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let n = " << wave.n << ";\n";
            out << "    let s = [];\n";
            out << "    for (let i = 0; i < n; i++) s.push([i * 2, i * 2 + 1]);\n";
            out << "    if (dispatch(s, 0) !== 0) allCorrect = false;\n";
            out << "    ops = n;\n";
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
