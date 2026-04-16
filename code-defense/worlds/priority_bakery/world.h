#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace priority_bakery {
    constexpr const char* WORLD_NAME = "The Priority Bakery";
    constexpr const char* WORLD_DESC = "Heaps — kth largest, heap sort, top-k sum, k-way merge, running median";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
