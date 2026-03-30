#include "battle.h"
#include <string>

static float battle_timer = 0.0f;
static bool battle_initialized = false;

void initBattleScreen(GameState& state, Renderer& renderer) {
    const auto& wave = state.waves[state.current_wave];
    renderer.initCreeps(wave.n);
    battle_timer = 0.0f;
    battle_initialized = true;
}

void updateBattleScreen(GameState& state, Renderer& renderer, float dt) {
    if (!battle_initialized) return;

    battle_timer += dt;

    bool timed_out = (!state.last_result.compiled) ||
                     (state.last_result.error_msg.find("Time limit") != std::string::npos);
    bool wrong = !state.wave_passed && !timed_out;

    renderer.updateCreeps(dt, state.wave_passed, timed_out, wrong);

    if (renderer.creepAnimationDone() || battle_timer > 3.5f) {
        if (!state.wave_passed) {
            state.lives--;
            state.failed_waves.push_back(state.waves[state.current_wave].id);
        }
        battle_initialized = false;
        state.current_screen = Screen::RESULTS;
    }
}

void drawBattleScreen(GameState& state, Renderer& renderer) {
    const auto& wave = state.waves[state.current_wave];

    std::string header = ">>> Wave " + std::to_string(wave.id) + " - BATTLE <<<";
    renderer.printCentered(1, header, COL_YELLOW, true);

    if (state.wave_passed) {
        renderer.printCentered(3, "Executing solution... creeps eliminated!", COL_GREEN, true);
    } else if (!state.last_result.compiled) {
        renderer.printCentered(3, "COMPILATION FAILED - Creeps charge through!", COL_RED, true);
    } else if (state.last_result.error_msg.find("Time limit") != std::string::npos) {
        renderer.printCentered(3, "TIME LIMIT EXCEEDED - Creeps overwhelm defenses!", COL_RED, true);
    } else {
        renderer.printCentered(3, "WRONG ANSWER - Creeps turn hostile!", COL_RED, true);
    }

    renderer.print(5, 2, "N = " + std::to_string(wave.n), COL_CYAN);

    std::string lives = "Lives: ";
    for (int i = 0; i < state.lives; i++) lives += "<3 ";
    renderer.print(5, renderer.cols() - (int)lives.size() - 2, lives, COL_RED, true);

    renderer.drawHLine(7, 0, renderer.cols(), COL_WHITE);

    renderer.drawCreeps();
}
