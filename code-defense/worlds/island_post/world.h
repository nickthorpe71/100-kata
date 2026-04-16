#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace island_post {
    constexpr const char* WORLD_NAME = "The Island Post";
    constexpr const char* WORLD_DESC = "Graphs — connected components, reachability, shortest path, cycle detection, topological sort";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
