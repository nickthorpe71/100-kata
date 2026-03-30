#pragma once
#include "../game.h"
#include "../renderer.h"
#include "../timer.h"

void handleCodingScreen(GameState& state, WriteTimer& timer, int key, const std::string& player_dir);
void drawCodingScreen(GameState& state, Renderer& renderer, WriteTimer& timer);
