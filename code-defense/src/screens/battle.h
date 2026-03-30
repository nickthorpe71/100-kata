#pragma once
#include "../game.h"
#include "../renderer.h"

void initBattleScreen(GameState& state, Renderer& renderer);
void updateBattleScreen(GameState& state, Renderer& renderer, float dt);
void drawBattleScreen(GameState& state, Renderer& renderer);
