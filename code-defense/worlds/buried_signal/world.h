#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace buried_signal {
    constexpr const char* WORLD_NAME = "The Buried Signal";
    constexpr const char* WORLD_DESC = "Binary Search — exact match, bounds, bitonic peak, rotated arrays";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
