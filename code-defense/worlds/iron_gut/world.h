#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace iron_gut {
    constexpr const char* WORLD_NAME = "The Iron Gut";
    constexpr const char* WORLD_DESC = "Stacks — brackets, postfix eval, path simplification, and monotonic stacks";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
