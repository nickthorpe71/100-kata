#pragma once
#include "../game.h"
#include "../renderer.h"
#include "../timer.h"

void handleWaveIntroScreen(GameState& state, int key);
void drawWaveIntroScreen(GameState& state, Renderer& renderer, WriteTimer& timer);
