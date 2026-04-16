#pragma once
#include "../../src/game.h"
#include <vector>
#include <string>

namespace spreading_rot {
    constexpr const char* WORLD_NAME = "The Spreading Rot";
    constexpr const char* WORLD_DESC = "Queues — FIFO processing, BFS traversal, level-order, and shortest path";

    std::vector<WaveDef> loadWaves();
    void generateStub(const WaveDef& wave, const std::string& player_dir, Language lang);
    void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
}
