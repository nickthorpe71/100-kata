#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace hash_maps {
    constexpr const char* WORLD_NAME = "Hash Valley";
    constexpr const char* WORLD_DESC = "Hash Maps — frequency maps, complement lookup, and map-of-maps";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
