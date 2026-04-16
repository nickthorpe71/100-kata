#include "wave_intro.h"
#include <string>

static const std::string& promptText(const WaveDef& wave) {
    return wave.clear_prompt.empty() ? wave.description : wave.clear_prompt;
}

void handleWaveIntroScreen(GameState& state, int key) {
    if (key == '\n' || key == KEY_ENTER || key == ' ') {
        const WaveDef& wave = state.waves[state.current_wave];
        if (!wave.quiz.questions.empty()) {
            state.current_screen = Screen::QUIZ;
        } else {
            state.current_screen = Screen::CODING;
        }
    }
    if (key == 'q') {
        state.quit = true;
    }
}

void drawWaveIntroScreen(GameState& state, Renderer& r, WriteTimer& /*timer*/) {
    const WaveDef& wave = state.waves[state.current_wave];

    std::string title = ">>> WAVE " + std::to_string(wave.id) + " INCOMING <<<";
    r.printCentered(2, title, COL_YELLOW, true);

    // Intro screen no longer consumes write time. Show the starting budget instead.
    int total_seconds = wave.write_time_s + 120;
    int minutes = total_seconds / 60;
    int seconds = total_seconds % 60;
    char tbuf[64];
    snprintf(tbuf, sizeof(tbuf), "[ write time starts at %d:%02d ]", minutes, seconds);
    r.printCentered(0, tbuf, COL_GREEN, true);

    r.drawHLine(4, 2, r.cols() - 4, COL_WHITE);

    r.print(6, 2, wave.name, COL_WHITE, true);

    r.printWrapped(8, 2, promptText(wave), r.cols() - 4, COL_WHITE);
    r.printWrapped(13, 2, "Examples: " + wave.examples, r.cols() - 4, COL_CYAN);

    if (!wave.flavor_text.empty()) {
        r.printWrapped(18, 2, wave.flavor_text, r.cols() - 4, COL_GRAY);
    }

    r.print(24, 2, "N = " + std::to_string(wave.n), COL_CYAN, true);
    r.print(25, 2, "Time Limit: " + std::to_string(wave.time_limit_ms) + "ms", COL_WHITE);
    r.print(26, 2, "Signature: " + wave.func_signature, COL_GREEN);

    // Lives
    std::string lives = "Lives: ";
    for (int i = 0; i < state.lives; i++) lives += "<3 ";
    r.print(28, 2, lives, COL_RED, true);

    r.drawHLine(30, 2, r.cols() - 4, COL_WHITE);

    std::string ext = (state.language == Language::JS) ? "solution.js" : "solution.cpp";
    r.print(32, 2, ext + " is ready - open it in your editor now", COL_WHITE);
    if (!wave.quiz.questions.empty()) {
        r.printCentered(34, "[ENTER] Begin Approach Check    [Q] Quit", COL_YELLOW, true);
    } else {
        r.printCentered(34, "[ENTER] Begin Coding    [Q] Quit", COL_YELLOW, true);
    }
}
