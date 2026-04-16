#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace surveyors_map {
    constexpr const char* WORLD_NAME = "The Surveyor's Map";
    constexpr const char* WORLD_DESC = "Union Find — connected components, redundant edges, earliest connection";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
