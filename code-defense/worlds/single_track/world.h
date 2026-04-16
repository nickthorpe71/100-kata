#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace single_track {
    constexpr const char* WORLD_NAME = "The Single Track";
    constexpr const char* WORLD_DESC = "Intervals — merge, insert, platform count, overlap check, and scheduling";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
