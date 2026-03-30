#pragma once
#include <string>
#include <vector>

enum class Screen {
    TITLE,
    WAVE_INTRO,
    CODING,
    BATTLE,
    RESULTS,
    GAME_OVER
};

struct WaveDef {
    int id;
    std::string name;
    std::string description;
    std::string examples;
    std::string constraints;
    int n;
    int time_limit_ms;
    int write_time_s;
    std::string stub_file;     // path to template stub
    std::string runner_file;   // will be generated
    std::string func_signature;
};

struct ExecutionResult {
    bool compiled;
    bool correct;
    int time_ms;
    int operations;
    std::string error_msg;
};

struct GameState {
    Screen current_screen = Screen::TITLE;
    int lives = 5;
    int current_wave = 0;       // 0-indexed into wave list
    int total_waves = 5;        // phase 1: 5 waves
    float write_timer = 0.0f;   // seconds remaining
    bool timer_paused = false;
    bool wave_passed = false;
    ExecutionResult last_result = {};
    std::vector<int> failed_waves;  // study list
    std::vector<WaveDef> waves;
    bool quit = false;
    bool game_started = false;
};
