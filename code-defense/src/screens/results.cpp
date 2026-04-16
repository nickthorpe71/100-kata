#include "results.h"
#include <string>

void handleResultsScreen(GameState& state, int key) {
    if (key == '\n' || key == KEY_ENTER || key == ' ') {
        if (state.lives <= 0) {
            state.current_screen = Screen::GAME_OVER;
        } else if (state.current_wave + 1 >= state.world_end_wave) {
            // Marathon mode: advance to next world if there is one
            if (state.mode == GameMode::MARATHON && state.world_end_wave < (int)state.waves.size()) {
                state.current_wave = state.world_end_wave;
                // Find the new world's end
                for (const auto& world : state.worlds) {
                    if (world.start_index == state.current_wave) {
                        state.world_end_wave = world.start_index + world.wave_count;
                        break;
                    }
                }
                state.current_screen = Screen::WAVE_INTRO;
            } else {
                state.current_screen = Screen::GAME_OVER;
            }
        } else {
            state.current_wave++;
            state.current_screen = Screen::WAVE_INTRO;
        }
    }
    if (key == 'q') {
        state.current_screen = Screen::GAME_OVER;
    }
}

void drawResultsScreen(GameState& state, Renderer& r) {
    const auto& wave = state.waves[state.current_wave];

    if (state.wave_passed) {
        r.printCentered(2, "=== WAVE CLEARED! ===", COL_GREEN, true);
    } else {
        r.printCentered(2, "=== WAVE FAILED! ===", COL_RED, true);
    }

    std::string wave_label = wave.name;
    if (!wave.world_name.empty()) {
        wave_label = "[" + wave.world_name + "] " + wave.name;
    }
    r.print(5, 2, wave_label, COL_WHITE, true);

    if (state.last_result.compiled) {
        r.print(7, 2, "Execution time: " + std::to_string(state.last_result.time_ms) + "ms", COL_WHITE);
        r.print(8, 2, "Time limit:     " + std::to_string(wave.time_limit_ms) + "ms", COL_GRAY);
        r.print(9, 2, "Operations:     " + std::to_string(state.last_result.operations), COL_WHITE);

        if (state.last_result.correct) {
            r.print(11, 2, "Result: CORRECT", COL_GREEN, true);
        } else {
            r.print(11, 2, "Result: WRONG ANSWER", COL_RED, true);
        }

        if (state.last_result.time_ms > wave.time_limit_ms) {
            r.print(12, 2, "TIME LIMIT EXCEEDED", COL_RED, true);
        }
    } else {
        r.print(7, 2, "COMPILATION ERROR", COL_RED, true);
        r.printWrapped(9, 2, state.last_result.error_msg, r.cols() - 4, COL_GRAY);
    }

    // Lives
    std::string lives = "Lives remaining: ";
    for (int i = 0; i < state.lives; i++) lives += "<3 ";
    r.print(15, 2, lives, state.lives <= 2 ? COL_RED : COL_WHITE, true);

    r.drawHLine(17, 0, r.cols(), COL_WHITE);

    if (state.lives <= 0) {
        r.printCentered(19, "NO LIVES REMAINING", COL_RED, true);
        r.printCentered(21, "[ENTER] Game Over", COL_YELLOW, true);
    } else if (state.current_wave + 1 >= state.world_end_wave) {
        if (state.mode == GameMode::MARATHON && state.world_end_wave < (int)state.waves.size()) {
            const auto& next = state.waves[state.world_end_wave];
            r.printCentered(19, "WORLD COMPLETE! Next: " + next.world_name, COL_GREEN, true);
            r.printCentered(21, "[ENTER] Next World    [Q] Save & Quit", COL_YELLOW, true);
        } else {
            r.printCentered(19, "ALL WAVES COMPLETE!", COL_YELLOW, true);
            r.printCentered(21, "[ENTER] Finish", COL_YELLOW, true);
        }
    } else {
        const auto& next = state.waves[state.current_wave + 1];
        std::string next_label = next.name;
        if (!next.world_name.empty()) {
            next_label = "[" + next.world_name + "] " + next.name;
        }
        r.print(19, 2, "Next: " + next_label, COL_YELLOW);
        r.print(20, 2, "N = " + std::to_string(next.n), COL_GRAY);
        r.printCentered(22, "[ENTER] Next Wave    [Q] Quit", COL_YELLOW, true);
    }
}
