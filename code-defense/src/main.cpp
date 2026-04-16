#include "game.h"
#include "renderer.h"
#include "wave.h"
#include "compiler.h"
#include "timer.h"
#include "screens/title.h"
#include "screens/world_select.h"
#include "screens/wave_intro.h"
#include "screens/quiz.h"
#include "screens/coding.h"
#include "screens/battle.h"
#include "screens/results.h"
#include <string>
#include <cstdio>
#include <ctime>
#include <filesystem>
#include <fstream>
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

static std::string getSavePath() {
    return getBaseDir() + "/save.dat";
}

static void saveGame(const GameState& state) {
    std::ofstream out(getSavePath());
    if (!out.is_open()) return;
    out << (int)state.mode << "\n";
    out << state.current_wave << "\n";
    out << state.total_waves << "\n";
    out << state.world_end_wave << "\n";
    out << state.lives << "\n";
    // Write current world/mode label
    const auto& wave = state.waves[state.current_wave];
    std::string label;
    if (state.mode == GameMode::MARATHON) {
        label = "Marathon - " + wave.world_name;
    } else if (state.mode == GameMode::SINGLE_WORLD) {
        label = wave.world_name;
    } else {
        label = "Special Mode";
    }
    out << label << "\n";
    out << (int)state.failed_waves.size() << "\n";
    for (int wid : state.failed_waves) out << wid << "\n";
    out.close();
}

static bool loadGame(GameState& state) {
    std::ifstream in(getSavePath());
    if (!in.is_open()) return false;
    int mode_int;
    if (!(in >> mode_int)) return false;
    state.save.mode = (GameMode)mode_int;
    if (!(in >> state.save.current_wave)) return false;
    if (!(in >> state.save.total_waves)) return false;
    if (!(in >> state.save.world_end_wave)) return false;
    if (!(in >> state.save.lives)) return false;
    in.ignore();
    std::getline(in, state.save.label);
    int num_failed;
    if (!(in >> num_failed)) return false;
    state.save.failed_waves.clear();
    for (int i = 0; i < num_failed; i++) {
        int wid;
        if (!(in >> wid)) return false;
        state.save.failed_waves.push_back(wid);
    }
    in.close();
    return true;
}

static void deleteSave() {
    fs::remove(getSavePath());
}

// Find a wave by ID in the waves vector
static const WaveDef* findWave(const GameState& state, int wid) {
    for (const auto& w : state.waves) {
        if (w.id == wid) return &w;
    }
    return nullptr;
}

static void drawGameOverScreen(GameState& state, Renderer& r) {
    bool at_world_end = state.current_wave + 1 >= state.world_end_wave;
    bool marathon_continues = state.mode == GameMode::MARATHON && at_world_end &&
                              state.world_end_wave < (int)state.waves.size() && state.lives > 0;
    bool is_true_end = state.lives <= 0 || (at_world_end && !marathon_continues);

    // Mid-run pause screen
    if (!is_true_end) {
        r.printCentered(2, "=================================", COL_YELLOW, true);
        r.printCentered(3, "           PAUSED                ", COL_YELLOW, true);
        r.printCentered(4, "=================================", COL_YELLOW, true);

        const auto& wave = state.waves[state.current_wave];
        r.print(7, 2, "Current: [" + wave.world_name + "] " + wave.name, COL_WHITE);
        r.print(8, 2, "Lives: " + std::to_string(state.lives), COL_WHITE);

        r.printCentered(r.rows() / 2, "[ENTER] Continue    [Q] Save & Quit", COL_YELLOW, true);
        return;
    }

    bool victory = state.lives > 0;

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
        r.print(10, 2, "STUDY LIST - Select a problem to review:", COL_YELLOW, true);
        int row = 12;
        for (int i = 0; i < (int)state.failed_waves.size(); i++) {
            const WaveDef* w = findWave(state, state.failed_waves[i]);
            if (!w) continue;
            std::string label = "[" + w->world_name + "] " + w->name;
            if (i == state.selected_study) {
                r.print(row, 4, "> " + label, COL_YELLOW, true);
            } else {
                r.print(row, 4, "  " + label, COL_GRAY);
            }
            row++;
        }
        r.printCentered(r.rows() - 2, "[UP/DOWN] Navigate    [ENTER] Study    [Q] World Select", COL_GRAY);
    } else {
        r.print(10, 2, "Perfect run! No failed waves.", COL_GREEN, true);
        r.printCentered(r.rows() - 2, "[ENTER] World Select    [Q] Quit", COL_YELLOW, true);
    }
}

static void goToWorldSelect(GameState& state, bool clean_save = true) {
    if (state.random_mode) {
        state.waves.resize(state.waves.size() - state.total_waves);
        state.random_mode = false;
    }
    if (clean_save) deleteSave();
    state.current_screen = Screen::WORLD_SELECT;
    state.game_started = false;
}

static void handleGameOverScreen(GameState& state, int key) {
    int num_failed = (int)state.failed_waves.size();
    bool at_world_end = state.current_wave + 1 >= state.world_end_wave;
    bool marathon_continues = state.mode == GameMode::MARATHON && at_world_end &&
                              state.world_end_wave < (int)state.waves.size() && state.lives > 0;
    bool is_true_end = state.lives <= 0 || (at_world_end && !marathon_continues);

    // Mid-run quit: offer to save and exit
    if (!is_true_end) {
        if (key == 'q' || key == 27) {
            if (state.mode == GameMode::MARATHON || state.mode == GameMode::SINGLE_WORLD) {
                saveGame(state);
                state.has_save = loadGame(state);
            }
            goToWorldSelect(state, false); // don't delete save we just created
        } else if (key == '\n' || key == KEY_ENTER) {
            state.current_wave++;
            state.current_screen = Screen::WAVE_INTRO;
        }
        return;
    }

    if (key == 'q' || key == 27) {
        goToWorldSelect(state);
    } else if (num_failed > 0 && (key == KEY_UP || key == 'k')) {
        state.selected_study = (state.selected_study - 1 + num_failed) % num_failed;
    } else if (num_failed > 0 && (key == KEY_DOWN || key == 'j')) {
        state.selected_study = (state.selected_study + 1) % num_failed;
    } else if (key == '\n' || key == KEY_ENTER) {
        if (num_failed > 0) {
            state.current_screen = Screen::STUDY;
        } else {
            goToWorldSelect(state);
        }
    }
}

static void drawStudyScreen(GameState& state, Renderer& r) {
    if (state.selected_study >= (int)state.failed_waves.size()) return;
    const WaveDef* w = findWave(state, state.failed_waves[state.selected_study]);
    if (!w) return;

    r.printCentered(1, "=================================", COL_CYAN, true);
    r.printCentered(2, "           STUDY MODE            ", COL_CYAN, true);
    r.printCentered(3, "=================================", COL_CYAN, true);

    r.print(5, 2, "[" + w->world_name + "] " + w->name, COL_YELLOW, true);
    const std::string& study_prompt = w->clear_prompt.empty() ? w->description : w->clear_prompt;
    r.print(7, 2, study_prompt, COL_WHITE);

    r.drawHLine(9, 0, r.cols(), COL_GRAY);

    if (!w->writeup.empty()) {
        r.print(11, 2, "APPROACH:", COL_GREEN, true);
        r.printWrapped(13, 2, w->writeup, r.cols() - 4, COL_WHITE);
    } else {
        r.print(11, 2, "No writeup available yet for this wave.", COL_GRAY);
    }

    r.printCentered(r.rows() - 2, "[Q/ESC] Back to Study List", COL_GRAY);
}

static void handleStudyScreen(GameState& state, int key) {
    if (key == 'q' || key == 27 || key == '\n' || key == KEY_ENTER) {
        state.current_screen = Screen::GAME_OVER;
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

    // Build world list from wave data
    for (int i = 0; i < (int)state.waves.size(); i++) {
        if (state.worlds.empty() || state.worlds.back().name != state.waves[i].world_name) {
            state.worlds.push_back({state.waves[i].world_name, state.waves[i].world_description, i, 1});
        } else {
            state.worlds.back().wave_count++;
        }
    }

    // Load save if one exists
    state.has_save = loadGame(state);

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
                case Screen::WORLD_SELECT:
                    handleWorldSelectScreen(state, key);
                    break;
                case Screen::WAVE_INTRO:
                    handleWaveIntroScreen(state, key);
                    break;
                case Screen::CODING:
                    handleCodingScreen(state, write_timer, key, PLAYER_DIR);
                    break;
                case Screen::QUIZ:
                    handleQuizScreen(state, key);
                    break;
                case Screen::BATTLE:
                    break;
                case Screen::RESULTS:
                    handleResultsScreen(state, key);
                    break;
                case Screen::GAME_OVER:
                    handleGameOverScreen(state, key);
                    break;
                case Screen::STUDY:
                    handleStudyScreen(state, key);
                    break;
            }
        }

        // Handle screen transitions after input
        if (state.current_screen != prev_screen) {
            if (state.current_screen == Screen::WAVE_INTRO) {
                // In random mode, delete solution.cpp to force a fresh stub
                if (state.random_mode) {
                    std::string ext = (state.language == Language::JS) ? ".js" : ".cpp";
                    fs::remove(PLAYER_DIR + "/solution" + ext);
                }
                // Start timer + generate files as soon as the problem is shown
                const auto& wave = state.waves[state.current_wave];
                generateSolutionStub(wave, PLAYER_DIR, state.language);
                generateRunner(wave, PLAYER_DIR, state.language);
                state.current_quiz_question = 0;
                state.selected_quiz_option = 0;
                state.quiz_answers.clear();
                state.quiz_feedback.clear();
                state.last_quiz_answer_correct = true;
            }
            if (state.current_screen == Screen::CODING) {
                const auto& wave = state.waves[state.current_wave];
                write_timer.start((float)wave.write_time_s + 120.0f);
            }
            if (state.current_screen == Screen::BATTLE) {
                battle_needs_init = true;
            }
            prev_screen = state.current_screen;
        }

        // Update
        switch (state.current_screen) {
            case Screen::CODING:
                write_timer.update(dt);
                if (write_timer.expired()) {
                    const auto& wave = state.waves[state.current_wave];
                    ExecutionResult result = compileAndRun(PLAYER_DIR, false, wave.time_limit_ms, state.language);
                    state.last_result = result;
                    state.wave_passed = result.compiled && result.correct &&
                                        result.time_ms <= wave.time_limit_ms;
                    state.current_screen = Screen::BATTLE;
                }
                break;
            case Screen::WAVE_INTRO:
            case Screen::QUIZ:
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
            case Screen::WORLD_SELECT:
                drawWorldSelectScreen(state, renderer);
                break;
            case Screen::WAVE_INTRO:
                drawWaveIntroScreen(state, renderer, write_timer);
                break;
            case Screen::QUIZ:
                drawQuizScreen(state, renderer);
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
            case Screen::STUDY:
                drawStudyScreen(state, renderer);
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
