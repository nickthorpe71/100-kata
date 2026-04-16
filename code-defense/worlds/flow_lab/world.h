#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace flow_lab {
    constexpr const char* WORLD_NAME = "The Flow Lab";
    constexpr const char* WORLD_DESC = "Sliding Window + Hash Map — distinct value windows, minimum covering window";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
