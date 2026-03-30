#include "game.h"
#include "renderer.h"
#include "wave.h"
#include "compiler.h"
#include "timer.h"
#include "screens/title.h"
#include "screens/wave_intro.h"
#include "screens/coding.h"
#include "screens/battle.h"
#include "screens/results.h"
#include <string>
#include <cstdio>
#include <ctime>
#include <filesystem>
#include <climits>
#include <unistd.h>

namespace fs = std::filesystem;

static std::string getBaseDir() {
    char buf[PATH_MAX];
    ssize_t len = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
    if (len > 0) {
        buf[len] = '\0';
        return fs::path(buf).parent_path().string();
    }
    return ".";
}

static std::string PLAYER_DIR;

static void drawGameOverScreen(GameState& state, Renderer& r) {
    bool victory = state.lives > 0 && state.current_wave + 1 >= state.total_waves;

    if (victory) {
        r.printCentered(2, "=================================", COL_GREEN, true);
        r.printCentered(3, "          VICTORY!               ", COL_GREEN, true);
        r.printCentered(4, "=================================", COL_GREEN, true);
        r.printCentered(6, "All waves cleared!", COL_YELLOW);
    } else {
        r.printCentered(2, "=================================", COL_RED, true);
        r.printCentered(3, "          GAME OVER              ", COL_RED, true);
        r.printCentered(4, "=================================", COL_RED, true);
        int passed = state.current_wave + 1 - (int)state.failed_waves.size();
        r.printCentered(6, "Waves passed: " + std::to_string(passed) +
                           " / " + std::to_string(state.total_waves), COL_WHITE);
    }

    r.print(8, 2, "Lives remaining: " + std::to_string(state.lives), COL_WHITE);

    if (!state.failed_waves.empty()) {
        r.print(10, 2, "STUDY LIST - Problems to review:", COL_YELLOW, true);
        int row = 12;
        for (int wid : state.failed_waves) {
            for (const auto& w : state.waves) {
                if (w.id == wid) {
                    r.print(row, 4, "Wave " + std::to_string(w.id) + ": " + w.name, COL_GRAY);
                    row++;
                    break;
                }
            }
        }
    } else {
        r.print(10, 2, "Perfect run! No failed waves.", COL_GREEN, true);
    }

    r.printCentered(r.rows() - 2, "[ENTER] Play Again    [Q] Quit", COL_YELLOW, true);
}

static void handleGameOverScreen(GameState& state, int key) {
    if (key == 'q' || key == 27) {
        state.quit = true;
    } else if (key == '\n' || key == KEY_ENTER) {
        state.current_screen = Screen::TITLE;
        state.game_started = false;
    }
}

static double now_seconds() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1e9;
}

int main() {
    PLAYER_DIR = getBaseDir() + "/player";
    fs::create_directories(PLAYER_DIR);

    Renderer renderer;
    if (!renderer.init()) {
        fprintf(stderr, "Failed to initialize renderer\n");
        return 1;
    }

    GameState state;
    state.waves = loadWaves();
    state.total_waves = (int)state.waves.size();

    WriteTimer write_timer;
    bool battle_needs_init = true;
    Screen prev_screen = Screen::TITLE;

    double last_time = now_seconds();

    while (!state.quit) {
        double now = now_seconds();
        float dt = (float)(now - last_time);
        last_time = now;

        // Input — only one key per frame to prevent skipping screens
        int key = getch();
        // Flush any remaining buffered keys
        if (key != ERR) flushinp();

        if (key != ERR) {
            switch (state.current_screen) {
                case Screen::TITLE:
                    handleTitleScreen(state, key);
                    break;
                case Screen::WAVE_INTRO:
                    handleWaveIntroScreen(state, key);
                    break;
                case Screen::CODING:
                    handleCodingScreen(state, write_timer, key, PLAYER_DIR);
                    break;
                case Screen::BATTLE:
                    break;
                case Screen::RESULTS:
                    handleResultsScreen(state, key);
                    break;
                case Screen::GAME_OVER:
                    handleGameOverScreen(state, key);
                    break;
            }
        }

        // Handle screen transitions after input
        if (state.current_screen != prev_screen) {
            if (state.current_screen == Screen::WAVE_INTRO) {
                // Start timer + generate files as soon as the problem is shown
                const auto& wave = state.waves[state.current_wave];
                generateSolutionStub(wave, PLAYER_DIR);
                generateRunner(wave, PLAYER_DIR);
                write_timer.start((float)wave.write_time_s + 120.0f);
            }
            if (state.current_screen == Screen::BATTLE) {
                battle_needs_init = true;
            }
            prev_screen = state.current_screen;
        }

        // Update
        switch (state.current_screen) {
            case Screen::WAVE_INTRO:
                write_timer.update(dt);
                break;
            case Screen::CODING:
                write_timer.update(dt);
                if (write_timer.expired()) {
                    const auto& wave = state.waves[state.current_wave];
                    ExecutionResult result = compileAndRun(PLAYER_DIR, false, wave.time_limit_ms);
                    state.last_result = result;
                    state.wave_passed = result.compiled && result.correct &&
                                        result.time_ms <= wave.time_limit_ms;
                    state.current_screen = Screen::BATTLE;
                }
                break;
            case Screen::BATTLE:
                if (battle_needs_init) {
                    initBattleScreen(state, renderer);
                    battle_needs_init = false;
                }
                updateBattleScreen(state, renderer, dt);
                break;
            default:
                break;
        }

        // Draw
        renderer.clear();

        switch (state.current_screen) {
            case Screen::TITLE:
                drawTitleScreen(state, renderer);
                break;
            case Screen::WAVE_INTRO:
                drawWaveIntroScreen(state, renderer, write_timer);
                break;
            case Screen::CODING:
                drawCodingScreen(state, renderer, write_timer);
                break;
            case Screen::BATTLE:
                drawBattleScreen(state, renderer);
                break;
            case Screen::RESULTS:
                drawResultsScreen(state, renderer);
                break;
            case Screen::GAME_OVER:
                drawGameOverScreen(state, renderer);
                break;
        }

        renderer.present();

        // ~30fps, avoid busy-spinning
        struct timespec sleep_ts = { 0, 33000000 };
        nanosleep(&sleep_ts, nullptr);
    }

    renderer.shutdown();
    return 0;
}
