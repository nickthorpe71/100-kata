#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace card_catalog {
    constexpr const char* WORLD_NAME = "The Card Catalog";
    constexpr const char* WORLD_DESC = "Tries — prefix search, exact match, longest common prefix, node counting, word break";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
