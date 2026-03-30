#include "wave_intro.h"
#include <string>

void handleWaveIntroScreen(GameState& state, int key) {
    if (key == '\n' || key == KEY_ENTER || key == ' ') {
        state.current_screen = Screen::CODING;
    }
    if (key == 'q') {
        state.quit = true;
    }
}

void drawWaveIntroScreen(GameState& state, Renderer& r, WriteTimer& timer) {
    const WaveDef& wave = state.waves[state.current_wave];

    std::string title = ">>> WAVE " + std::to_string(wave.id) + " INCOMING <<<";
    r.printCentered(2, title, COL_YELLOW, true);

    // Timer
    float remaining = timer.remaining();
    int minutes = (int)remaining / 60;
    int seconds = (int)remaining % 60;
    char tbuf[32];
    snprintf(tbuf, sizeof(tbuf), "[ %d:%02d ]", minutes, seconds);
    r.printCentered(0, tbuf, COL_GREEN, true);

    r.drawHLine(4, 2, r.cols() - 4, COL_WHITE);

    r.print(6, 2, wave.name, COL_WHITE, true);

    r.printWrapped(8, 2, wave.description, r.cols() - 4, COL_WHITE);

    r.print(12, 2, "N = " + std::to_string(wave.n), COL_CYAN, true);
    r.print(13, 2, "Time Limit: " + std::to_string(wave.time_limit_ms) + "ms", COL_WHITE);

    r.print(15, 2, "Signature: " + wave.func_signature, COL_GREEN);

    // Lives
    std::string lives = "Lives: ";
    for (int i = 0; i < state.lives; i++) lives += "<3 ";
    r.print(17, 2, lives, COL_RED, true);

    r.drawHLine(19, 2, r.cols() - 4, COL_WHITE);

    r.print(21, 2, "solution.cpp is ready - open it in your editor now", COL_WHITE);
    r.printCentered(23, "[ENTER] Begin Coding    [Q] Quit", COL_YELLOW, true);
}
