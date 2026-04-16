#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace bazaar {
    constexpr const char* WORLD_NAME = "The Bazaar";
    constexpr const char* WORLD_DESC = "Greedy — stock profit, jump game, minimum jumps, gas station, assignment";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
