#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace binary_telegraph {
    constexpr const char* WORLD_NAME = "The Binary Telegraph";
    constexpr const char* WORLD_DESC = "Bit Manipulation — set bits, XOR tricks, power of 2, bit reversal, missing number";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
