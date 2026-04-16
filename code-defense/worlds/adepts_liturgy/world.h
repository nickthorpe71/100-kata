#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace adepts_liturgy {
    constexpr const char* WORLD_NAME = "The Adept's Liturgy";
    constexpr const char* WORLD_DESC = "Strings — scan, purge, reverse, compress, and compare sacred text";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
