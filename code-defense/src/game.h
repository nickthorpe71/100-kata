#pragma once
#include <string>
#include <utility>
#include <vector>

enum class Language {
    CPP,
    JS
};

enum class Screen {
    TITLE,
    WORLD_SELECT,
    WAVE_INTRO,
    QUIZ,
    CODING,
    BATTLE,
    RESULTS,
    GAME_OVER,
    STUDY
};

enum class QuizMode {
    NONE,
    FULL,
    THEMED_LIGHT
};

struct QuizQuestion {
    std::string prompt;
    std::vector<std::string> options;
    int correct_index = -1;
};

struct WaveQuiz {
    QuizMode mode = QuizMode::NONE;
    std::vector<QuizQuestion> questions;
};

struct WorldInfo {
    std::string name;
    std::string description;
    int start_index;  // index into waves vector
    int wave_count;
};

struct WaveDef;

using StubGenFn = void(*)(const WaveDef&, const std::string&, Language);
using RunnerGenFn = void(*)(const WaveDef&, const std::string&, Language);

struct WaveDef {
    int id;
    std::string name;
    std::string world_name;
    std::string world_description;
    std::string description;
    std::string examples;
    std::string constraints;
    int n;
    int time_limit_ms;
    int write_time_s;
    std::string func_signature;
    std::string writeup;
    StubGenFn gen_stub = nullptr;
    RunnerGenFn gen_runner = nullptr;
    WaveQuiz quiz = {};
    std::string clear_prompt;
    std::string flavor_text;

    WaveDef(
        int id_ = 0,
        std::string name_ = "",
        std::string world_name_ = "",
        std::string world_description_ = "",
        std::string description_ = "",
        std::string examples_ = "",
        std::string constraints_ = "",
        int n_ = 0,
        int time_limit_ms_ = 0,
        int write_time_s_ = 0,
        std::string func_signature_ = "",
        std::string writeup_ = "",
        StubGenFn gen_stub_ = nullptr,
        RunnerGenFn gen_runner_ = nullptr,
        WaveQuiz quiz_ = {},
        std::string clear_prompt_ = "",
        std::string flavor_text_ = ""
    )
        : id(id_),
          name(std::move(name_)),
          world_name(std::move(world_name_)),
          world_description(std::move(world_description_)),
          description(std::move(description_)),
          examples(std::move(examples_)),
          constraints(std::move(constraints_)),
          n(n_),
          time_limit_ms(time_limit_ms_),
          write_time_s(write_time_s_),
          func_signature(std::move(func_signature_)),
          writeup(std::move(writeup_)),
          gen_stub(gen_stub_),
          gen_runner(gen_runner_),
          quiz(std::move(quiz_)),
          clear_prompt(std::move(clear_prompt_)),
          flavor_text(std::move(flavor_text_)) {}
};

struct ExecutionResult {
    bool compiled;
    bool correct;
    int time_ms;
    int operations;
    std::string error_msg;
};

enum class GameMode {
    NONE,
    SINGLE_WORLD,
    MARATHON,
    RANDOM_DRILL,
    GAUNTLET
};

struct SaveData {
    GameMode mode = GameMode::NONE;
    int current_wave = 0;
    int total_waves = 0;
    int world_end_wave = 0;
    int lives = 5;
    std::string label;              // e.g. "Marathon - The Closing Jaws" or "Hash Valley"
    std::vector<int> failed_waves;
};

struct GameState {
    Screen current_screen = Screen::TITLE;
    int lives = 5;
    int current_wave = 0;       // 0-indexed into wave list
    int total_waves = 0;
    float write_timer = 0.0f;   // seconds remaining
    bool timer_paused = false;
    bool wave_passed = false;
    ExecutionResult last_result = {};
    std::vector<int> failed_waves;  // study list
    std::vector<WaveDef> waves;
    std::vector<WorldInfo> worlds;
    int selected_world = 0;
    int world_end_wave = 0;       // one past the last wave index for current world
    bool quit = false;
    bool game_started = false;
    bool random_mode = false;
    int selected_study = 0;
    GameMode mode = GameMode::NONE;
    Language language = Language::CPP;
    SaveData save;
    bool has_save = false;
    int current_quiz_question = 0;
    int selected_quiz_option = 0;
    std::vector<int> quiz_answers;
    std::string quiz_feedback;
    bool last_quiz_answer_correct = true;
};
