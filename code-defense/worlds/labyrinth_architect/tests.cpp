#include "world.h"
#include "../../src/quiz_bank.h"
#include <fstream>

std::vector<WaveDef> labyrinth_architect::loadWaves() {
    std::vector<WaveDef> waves;

    waves.push_back({
        1801, "All Permutations", WORLD_NAME, WORLD_DESC,
        "Count the number of distinct permutations of the first target elements of blueprint. The blueprint may contain duplicates.",
        "Example: [1,2,3], target=3 -> 6\nExample: [1,1,2], target=3 -> 3\nExample: [1], target=1 -> 1\nExample: [1,2], target=2 -> 2",
        "1 <= target <= blueprint.size() <= 8",
        8, 2000, 300,
        "int buildLabyrinth(vector<int>& blueprint, int target)",
        "Swap-based backtracking. For position i, try swapping with each position j >= i, recurse on i+1, swap back. Handle duplicates by skipping if element already tried at position i. O(n!).", generateStub, generateRunner
    });

    waves.push_back({
        1802, "Valid Combinations", WORLD_NAME, WORLD_DESC,
        "Count the number of combinations of elements from blueprint that sum to target. Each element can be used at most once.",
        "Example: [10,1,2,7,6,1,5], target=8 -> 4 ({1,2,5},{1,7},{2,6},{1,1,6})\nExample: [1,2,3], target=7 -> 0\nExample: [1], target=1 -> 1",
        "1 <= blueprint.size() <= 30\n1 <= target <= 1000",
        30, 2000, 300,
        "int buildLabyrinth(vector<int>& blueprint, int target)",
        "Sort the array. Backtrack: for each element, include or exclude. Skip duplicates (if same value as previous and previous wasn't included). O(2^n).", generateStub, generateRunner
    });

    waves.push_back({
        1803, "Place the Queens", WORLD_NAME, WORLD_DESC,
        "N-Queens: place target queens on a target x target board so no two attack each other. Return the number of valid placements. blueprint is unused.",
        "Example: target=4 -> 2\nExample: target=1 -> 1\nExample: target=8 -> 92\nExample: target=5 -> 10",
        "1 <= target <= 12",
        12, 5000, 300,
        "int buildLabyrinth(vector<int>& blueprint, int target)",
        "Place queens column by column. For each row in current column, check if safe (no queen in same row, or diagonals). If safe, place and recurse. Backtrack on failure. O(n!).", generateStub, generateRunner
    });

    waves.push_back({
        1804, "Solve the Grid", WORLD_NAME, WORLD_DESC,
        "Sudoku-lite: blueprint encodes a target x target grid in row-major order (0 = empty). Fill empty cells with values 1..target so that no row or column contains a repeated value. Return 1 if solvable, 0 otherwise.",
        "Example: [0,0,0,0], target=2 -> 1 (solution exists: [[1,2],[2,1]])\nExample: [1,1,0,0], target=2 -> 0 (row 0 already has two 1s)",
        "1 <= target <= 9\nblueprint.size() == target * target",
        9, 5000, 300,
        "int buildLabyrinth(vector<int>& blueprint, int target)",
        "For each empty cell, try values 1..target. Check row and column constraints. If valid, place and recurse. Backtrack if no value works. Pruning: pick the most constrained cell first.", generateStub, generateRunner
    });

    waves.push_back({
        1805, "The Master Plan", WORLD_NAME, WORLD_DESC,
        "Count all valid arrangements of target pairs of parentheses (the target-th Catalan number). blueprint is unused.",
        "Example: target=1 -> 1\nExample: target=2 -> 2\nExample: target=3 -> 5\nExample: target=4 -> 14\nExample: target=5 -> 42",
        "1 <= target <= 14",
        14, 5000, 300,
        "int buildLabyrinth(vector<int>& blueprint, int target)",
        "Backtrack with balance tracking. At each position, you can place '(' if open < n, or ')' if close < open. Count complete strings of length 2n. Result is the nth Catalan number. O(4^n / sqrt(n)).", generateStub, generateRunner
    });

    quizbank::attachThemedQuiz(
        waves,
        "a backtracking approach",
        "Need to explore choices, undo them, and prune invalid partial states",
        "O(n) space"
    );
    waves.back().quiz = quizbank::makeFullQuiz(
        {
            "Backtracking with undo and constraint checks",
            "Binary search on the answer",
            "Heap of partial states only",
            "Union-Find of conflicting positions"
        },
        0,
        "Need to explore choices, undo them, and prune invalid partial states",
        quizbank::inferCombinedComplexity(waves.back().writeup, "O(n) space")
    );
    waves[0].clear_prompt = "Given the inputs described by the wave, return all valid permutations in the required format.";
    waves[0].flavor_text = "The Architect's apprentice studies the turning halls of a stone maze, where every ordering of keys opens a different corridor. He must enumerate every possible arrangement before the walls shift again.";
    waves[1].clear_prompt = "Given the inputs described by the wave, return all valid combinations that satisfy the target condition.";
    waves[1].flavor_text = "The architect's diagrams speak in combinations, not single moves. The apprentice tries one stone, then another, undoing every choice that leads to a dead hall.";
    waves[2].clear_prompt = "Given the board size or structure defined by the wave, return the valid N-Queens placements or count of placements.";
    waves[2].flavor_text = "High above the labyrinth, queens must be placed where none can threaten another across the open board. The apprentice tests one column at a time, retreating from every doomed arrangement.";
    waves[3].clear_prompt = "Given the puzzle grid or constraint structure defined by the wave, solve it using backtracking and return the required result.";
    waves[3].flavor_text = "This chamber is built from rules as much as stone. The apprentice advances, retracts, and tries again until the pattern finally locks into place.";
    waves[4].clear_prompt = "Given the boss wave's constraints, use backtracking to return the complete set or count of valid solutions.";
    waves[4].flavor_text = "At the labyrinth's heart lies the master plan, too intricate for blind force. The apprentice explores branching futures one by one, undoing every mistake before it hardens into architecture.";
    return waves;
}

void labyrinth_architect::generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
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

    out << "int buildLabyrinth(vector<int>& blueprint, int target);\n\n";

    out << "int main(int argc, char* argv[]) {\n";
    out << "    bool test_only = (argc > 1 && string(argv[1]) == \"--test\");\n";
    out << "    bool all_correct = true;\n";
    out << "    int ops = 0;\n\n";
    out << "    auto start = chrono::high_resolution_clock::now();\n\n";

    switch (wave.id) {
        case 1801: // All Permutations
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3};\n";
            out << "        if (buildLabyrinth(b, 3) != 6) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 1, 2};\n";
            out << "        if (buildLabyrinth(b, 3) != 3) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1};\n";
            out << "        if (buildLabyrinth(b, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2};\n";
            out << "        if (buildLabyrinth(b, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> b = {1, 2, 3, 4, 5, 6, 7, 8};\n";
            out << "        if (buildLabyrinth(b, 8) != 40320) all_correct = false;\n";
            out << "        ops = 40320;\n";
            out << "    } else { ops = 12; }\n";
            break;

        case 1802: // Valid Combinations
            out << "    {\n";
            out << "        vector<int> b = {10, 1, 2, 7, 6, 1, 5};\n";
            out << "        if (buildLabyrinth(b, 8) != 4) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3};\n";
            out << "        if (buildLabyrinth(b, 7) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1};\n";
            out << "        if (buildLabyrinth(b, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {2, 3, 5};\n";
            out << "        if (buildLabyrinth(b, 8) != 1) all_correct = false; // {3,5}\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> b = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};\n";
            out << "        if (buildLabyrinth(b, 20) != 64) all_correct = false;\n";
            out << "        ops = 1048576;\n";
            out << "    } else { ops = 14; }\n";
            break;

        case 1803: // Place the Queens
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 4) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 8) != 92) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 5) != 10) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 12) != 14200) all_correct = false;\n";
            out << "        ops = 14200;\n";
            out << "    } else { ops = 105; }\n";
            break;

        case 1804: // Solve the Grid
            out << "    {\n";
            out << "        vector<int> b = {0, 0, 0, 0};\n";
            out << "        if (buildLabyrinth(b, 2) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 1, 0, 0};\n";
            out << "        if (buildLabyrinth(b, 2) != 0) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};\n";
            out << "        if (buildLabyrinth(b, 4) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b = {1, 2, 3, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};\n";
            out << "        if (buildLabyrinth(b, 4) != 0) all_correct = false; // col 0 has two 1s\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        // 4x4 grid with corners filled\n";
            out << "        vector<int> b = {1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1};\n";
            out << "        if (buildLabyrinth(b, 4) != 1) all_correct = false;\n";
            out << "        ops = 256;\n";
            out << "    } else { ops = 20; }\n";
            break;

        case 1805: // The Master Plan (boss)
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 1) != 1) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 2) != 2) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 3) != 5) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 4) != 14) all_correct = false;\n";
            out << "    }\n";
            out << "    {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 5) != 42) all_correct = false;\n";
            out << "    }\n";
            out << "    if (!test_only) {\n";
            out << "        vector<int> b;\n";
            out << "        if (buildLabyrinth(b, 14) != 2674440) all_correct = false;\n";
            out << "        ops = 2674440;\n";
            out << "    } else { ops = 64; }\n";
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

    out << "const { buildLabyrinth } = require('./solution');\n";
    out << "const testOnly = process.argv.includes('--test');\n";
    out << "let allCorrect = true;\n";
    out << "let ops = 0;\n";
    out << "const start = process.hrtime.bigint();\n\n";

    switch (wave.id) {
        case 1801: // All Permutations
            out << "{\n";
            out << "    let b = [1, 2, 3];\n";
            out << "    if (buildLabyrinth(b, 3) !== 6) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 1, 2];\n";
            out << "    if (buildLabyrinth(b, 3) !== 3) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1];\n";
            out << "    if (buildLabyrinth(b, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2];\n";
            out << "    if (buildLabyrinth(b, 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let b = [1, 2, 3, 4, 5, 6, 7, 8];\n";
            out << "    if (buildLabyrinth(b, 8) !== 40320) allCorrect = false;\n";
            out << "    ops = 40320;\n";
            out << "} else { ops = 12; }\n";
            break;

        case 1802: // Valid Combinations
            out << "{\n";
            out << "    let b = [10, 1, 2, 7, 6, 1, 5];\n";
            out << "    if (buildLabyrinth(b, 8) !== 4) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, 3];\n";
            out << "    if (buildLabyrinth(b, 7) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1];\n";
            out << "    if (buildLabyrinth(b, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [2, 3, 5];\n";
            out << "    if (buildLabyrinth(b, 8) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let b = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20];\n";
            out << "    if (buildLabyrinth(b, 20) !== 64) allCorrect = false;\n";
            out << "    ops = 1048576;\n";
            out << "} else { ops = 14; }\n";
            break;

        case 1803: // Place the Queens
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 4) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 8) !== 92) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 5) !== 10) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 12) !== 14200) allCorrect = false;\n";
            out << "    ops = 14200;\n";
            out << "} else { ops = 105; }\n";
            break;

        case 1804: // Solve the Grid
            out << "{\n";
            out << "    let b = [0, 0, 0, 0];\n";
            out << "    if (buildLabyrinth(b, 2) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 1, 0, 0];\n";
            out << "    if (buildLabyrinth(b, 2) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];\n";
            out << "    if (buildLabyrinth(b, 4) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [1, 2, 3, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];\n";
            out << "    if (buildLabyrinth(b, 4) !== 0) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let b = [1,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,1];\n";
            out << "    if (buildLabyrinth(b, 4) !== 1) allCorrect = false;\n";
            out << "    ops = 256;\n";
            out << "} else { ops = 20; }\n";
            break;

        case 1805: // The Master Plan (boss)
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 1) !== 1) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 2) !== 2) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 3) !== 5) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 4) !== 14) allCorrect = false;\n";
            out << "}\n";
            out << "{\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 5) !== 42) allCorrect = false;\n";
            out << "}\n";
            out << "if (!testOnly) {\n";
            out << "    let b = [];\n";
            out << "    if (buildLabyrinth(b, 14) !== 2674440) allCorrect = false;\n";
            out << "    ops = 2674440;\n";
            out << "} else { ops = 64; }\n";
            break;
    }

    out << "\nconst end = process.hrtime.bigint();\n";
    out << "const ms = Number((end - start) / 1000000n);\n";
    out << "process.stdout.write(`${ms} ${ops}\\n`);\n";
    out << "process.exit(allCorrect ? 0 : 1);\n";

    out.close();
  }
}
