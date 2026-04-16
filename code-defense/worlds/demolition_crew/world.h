#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace demolition_crew {
    constexpr const char* WORLD_NAME = "The Demolition Crew";
    constexpr const char* WORLD_DESC = "Stack + Greedy — minimize strings, remove duplicates, decode, asteroid collision";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
