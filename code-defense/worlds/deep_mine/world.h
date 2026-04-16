#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace deep_mine {
    constexpr const char* WORLD_NAME = "The Deep Mine";
    constexpr const char* WORLD_DESC = "Binary Search + DP — bisect on answer with greedy/DP validation";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
