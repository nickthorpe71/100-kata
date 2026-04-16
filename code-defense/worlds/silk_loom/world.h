#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace silk_loom {
    constexpr const char* WORLD_NAME = "The Silk Loom";
    constexpr const char* WORLD_DESC = "Dynamic Programming (2D) — grid paths, min path sum, edit distance, knapsack, LCS";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
