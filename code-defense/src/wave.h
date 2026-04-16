#pragma once
#include "game.h"
#include <vector>
#include <string>

std::vector<WaveDef> loadWaves();
void generateSolutionStub(const WaveDef& wave, const std::string& player_dir, Language lang);
void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang);
