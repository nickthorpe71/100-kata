#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace moths_path {
    constexpr const char* WORLD_NAME = "The Moth's Path";
    constexpr const char* WORLD_DESC = "Dynamic Programming (1D) — climbing stairs, house robber, coin change, and LIS";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
