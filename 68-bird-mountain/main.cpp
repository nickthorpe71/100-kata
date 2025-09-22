#include <iostream>
#include <vector>
#include <climits>

#include <iostream>
#include <vector>
#include <climits>
#include <algorithm> // std::min

// r0 = starting row index, c0 = starting column index
int straight_line_distance_to_space(std::vector<std::string>& mountain, size_t r0, size_t c0) {
    const size_t H = mountain.size();
    const size_t W = mountain[0].size();

    int best = INT_MAX;

    // Up: vary r, keep c fixed
    for (int r = static_cast<int>(r0) - 1, steps = 1; r >= 0; --r, ++steps) {
        if (mountain[r][c0] == ' ') { best = std::min(best, steps); break; }
    }

    // Down: vary r, keep c fixed
    for (int r = static_cast<int>(r0) + 1, steps = 1; r < static_cast<int>(H); ++r, ++steps) {
        if (mountain[r][c0] == ' ') { best = std::min(best, steps); break; }
    }

    // Left: vary c, keep r fixed
    for (int c = static_cast<int>(c0) - 1, steps = 1; c >= 0; --c, ++steps) {
        if (mountain[r0][c] == ' ') { best = std::min(best, steps); break; }
    }

    // Right: vary c, keep r fixed
    for (int c = static_cast<int>(c0) + 1, steps = 1; c < static_cast<int>(W); ++c, ++steps) {
        if (mountain[r0][c] == ' ') { best = std::min(best, steps); break; }
    }

    return (best == INT_MAX) ? 0 : best; // 0 if no space seen in any straight line
}

int peak_height(std::vector<std::string>& mountain) {
    if (mountain.empty()) return 0;

    const size_t H = mountain.size();
    const size_t W = mountain[0].size();

    int max_peak = 0;

    for (size_t r = 0; r < H; ++r) {
        for (size_t c = 0; c < W; ++c) {
            if (mountain[r][c] == '^') {
                int d = straight_line_distance_to_space(mountain, r, c);
                if (d > max_peak) max_peak = d;
            }
        }
    }

    return max_peak;
}

void run_tests() {
    std::vector<std::string> mountain1 = {
        "  ^  ",
        " ^^^ ",
        "^^^^^"
    };
    std::cout << "Test 1: " << (peak_height(mountain1) == 2 ? "Passed" : "Failed") << std::endl;

    std::vector<std::string> mountain2 = {
        "^^^^^^        ",
        " ^^^^^^^^     ",
        "  ^^^^^^^     ",
        "  ^^^^^       ",
        "  ^^^^^^^^^^^ ",
        "  ^^^^^^      ",
        "  ^^^^        "
    };
    std::cout << "Test 2: " << (peak_height(mountain2) == 3 ? "Passed" : "Failed") << std::endl;
}

int main() {
    run_tests();
    return 0;
}