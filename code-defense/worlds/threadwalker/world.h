#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace threadwalker {
    constexpr const char* WORLD_NAME = "The Threadwalker";
    constexpr const char* WORLD_DESC = "Arrays — count, search, partition, sort, and rotate in place";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
