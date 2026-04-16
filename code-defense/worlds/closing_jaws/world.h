#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace closing_jaws {
    constexpr const char* WORLD_NAME = "The Closing Jaws";
    constexpr const char* WORLD_DESC = "Two Pointers — converge, count pairs, 3sum, partition, and merge";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
