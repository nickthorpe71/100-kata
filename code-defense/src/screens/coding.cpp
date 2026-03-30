#include "coding.h"
#include "../compiler.h"
#include <string>
#include <cstdio>

static std::string test_feedback;

void handleCodingScreen(GameState& state, WriteTimer& timer, int key,
                        const std::string& player_dir) {
    const WaveDef& wave = state.waves[state.current_wave];

    if (key == 't' || key == 'T') {
        timer.pause();
        test_feedback = "Compiling...";

        ExecutionResult result = compileAndRun(player_dir, true, wave.time_limit_ms);

        if (!result.compiled) {
            test_feedback = "COMPILE ERROR: " + result.error_msg;
        } else if (result.correct) {
            test_feedback = "Tests PASSED (" + std::to_string(result.time_ms) + "ms)";
        } else {
            test_feedback = "Tests FAILED: " + result.error_msg;
        }

        timer.resume();
    }

    if (key == 's' || key == 'S') {
        timer.pause();

        ExecutionResult result = compileAndRun(player_dir, false, wave.time_limit_ms);
        state.last_result = result;

        if (!result.compiled) {
            state.wave_passed = false;
            state.last_result.error_msg = "Compilation failed: " + result.error_msg;
        } else {
            state.wave_passed = result.correct && result.time_ms <= wave.time_limit_ms;
        }

        test_feedback = "";
        state.current_screen = Screen::BATTLE;
    }

    if (key == 'q') {
        state.quit = true;
    }
}

void drawCodingScreen(GameState& state, Renderer& r, WriteTimer& timer) {
    const WaveDef& wave = state.waves[state.current_wave];

    // Header bar: wave name left, timer center, lives right
    std::string header = "Wave " + std::to_string(wave.id) + ": " + wave.name;
    r.print(0, 2, header, COL_YELLOW, true);

    float remaining = timer.remaining();
    int minutes = (int)remaining / 60;
    int seconds = (int)remaining % 60;
    char tbuf[32];
    snprintf(tbuf, sizeof(tbuf), "[ %d:%02d ]", minutes, seconds);
    int timer_color = remaining < 30.0f ? COL_RED : COL_GREEN;
    r.printCentered(0, tbuf, timer_color, true);

    std::string lives = "Lives: ";
    for (int i = 0; i < state.lives; i++) lives += "<3 ";
    r.print(0, r.cols() - (int)lives.size() - 1, lives, COL_RED, true);

    r.drawHLine(1, 0, r.cols(), COL_WHITE);

    // Problem
    r.print(3, 2, "Problem:", COL_WHITE, true);
    r.printWrapped(4, 2, wave.description, r.cols() - 4, COL_WHITE);

    r.print(7, 2, "Examples:", COL_WHITE, true);
    r.printWrapped(8, 4, wave.examples, r.cols() - 6, COL_CYAN);

    r.print(12, 2, "Constraints:", COL_WHITE, true);
    r.printWrapped(13, 4, wave.constraints, r.cols() - 6, COL_GRAY);

    r.print(17, 2, "Signature:", COL_WHITE, true);
    r.print(18, 4, wave.func_signature, COL_GREEN, true);

    r.print(20, 2, "N = " + std::to_string(wave.n) + "  |  Time limit: " +
            std::to_string(wave.time_limit_ms) + "ms", COL_GRAY);

    r.drawHLine(22, 0, r.cols(), COL_WHITE);

    r.print(23, 2, "Edit player/solution.cpp in your editor, then:", COL_WHITE);

    // Test feedback
    if (!test_feedback.empty()) {
        int fb_color = COL_RED;
        if (test_feedback.find("PASSED") != std::string::npos) fb_color = COL_GREEN;
        else if (test_feedback.find("Compiling") != std::string::npos) fb_color = COL_YELLOW;
        r.printWrapped(25, 2, test_feedback, r.cols() - 4, fb_color);
    }

    // Controls at bottom
    int bot = r.rows() - 2;
    r.print(bot, 2, "[T] Test (small cases)    [S] Submit (full suite)    [Q] Quit", COL_YELLOW, true);
}
