#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace labyrinth_architect {
    constexpr const char* WORLD_NAME = "The Labyrinth Architect";
    constexpr const char* WORLD_DESC = "Backtracking — permutations, combinations, N-Queens, constraint satisfaction";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
