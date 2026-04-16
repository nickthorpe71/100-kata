#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace world_root {
    constexpr const char* WORLD_NAME = "The World Root";
    constexpr const char* WORLD_DESC = "Trees — traversal, depth, balance, path sums, symmetry, and LCA";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
