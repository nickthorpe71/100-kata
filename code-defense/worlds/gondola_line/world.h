#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace gondola_line {
    constexpr const char* WORLD_NAME = "The Gondola Line";
    constexpr const char* WORLD_DESC = "Linked Lists — traverse, reverse, fast/slow pointers, cycle detection, merge";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
