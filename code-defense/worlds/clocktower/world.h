#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace clocktower {
    constexpr const char* WORLD_NAME = "The Clocktower";
    constexpr const char* WORLD_DESC = "Recursion — base cases, branching, halving, subset decisions, and bidirectional data flow";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
