#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace arborist {
    constexpr const char* WORLD_NAME = "The Arborist";
    constexpr const char* WORLD_DESC = "Tree + DP — max path sum, house robber on tree, diameter, any-path maximum";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
