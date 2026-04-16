#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace porthole {
    constexpr const char* WORLD_NAME = "The Porthole";
    constexpr const char* WORLD_DESC = "Sliding Window — fixed and variable windows, sum, average, and range tracking";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
