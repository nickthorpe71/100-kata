#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace hanging_court {
    constexpr const char* WORLD_NAME = "The Hanging Court";
    constexpr const char* WORLD_DESC = "Sorting — bubble, insertion, merge sort, quicksort, and quickselect";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
