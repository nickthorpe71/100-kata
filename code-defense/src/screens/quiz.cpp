#include "quiz.h"
#include <string>

static const std::string& promptText(const WaveDef& wave) {
    return wave.clear_prompt.empty() ? wave.description : wave.clear_prompt;
}

static int optionCount(const GameState& state) {
    const WaveDef& wave = state.waves[state.current_wave];
    if (state.current_quiz_question < 0 ||
        state.current_quiz_question >= (int)wave.quiz.questions.size()) {
        return 0;
    }
    return (int)wave.quiz.questions[state.current_quiz_question].options.size();
}

void handleQuizScreen(GameState& state, int key) {
    const WaveDef& wave = state.waves[state.current_wave];
    if (wave.quiz.questions.empty()) {
        state.current_screen = Screen::CODING;
        return;
    }

    const QuizQuestion& question = wave.quiz.questions[state.current_quiz_question];
    int count = optionCount(state);
    if (count <= 0) return;

    if (key == KEY_UP || key == 'k') {
        state.selected_quiz_option =
            (state.selected_quiz_option - 1 + count) % count;
        state.quiz_feedback.clear();
        return;
    }

    if (key == KEY_DOWN || key == 'j') {
        state.selected_quiz_option =
            (state.selected_quiz_option + 1) % count;
        state.quiz_feedback.clear();
        return;
    }

    if (key == 'q' || key == 27) {
        state.quit = true;
        return;
    }

    if (key == '\n' || key == KEY_ENTER || key == ' ') {
        if (state.selected_quiz_option == question.correct_index) {
            if ((int)state.quiz_answers.size() <= state.current_quiz_question) {
                state.quiz_answers.resize(state.current_quiz_question + 1, -1);
            }
            state.quiz_answers[state.current_quiz_question] = state.selected_quiz_option;
            state.last_quiz_answer_correct = true;
            state.quiz_feedback = "Correct.";
            state.current_quiz_question++;
            state.selected_quiz_option = 0;

            if (state.current_quiz_question >= (int)wave.quiz.questions.size()) {
                state.quiz_feedback.clear();
                state.current_screen = Screen::CODING;
            }
        } else {
            state.last_quiz_answer_correct = false;
            state.quiz_feedback = "Incorrect. Try again.";
        }
    }
}

void drawQuizScreen(GameState& state, Renderer& r) {
    const WaveDef& wave = state.waves[state.current_wave];
    if (wave.quiz.questions.empty()) return;

    const QuizQuestion& question = wave.quiz.questions[state.current_quiz_question];

    r.printCentered(1, "=================================", COL_CYAN, true);
    r.printCentered(2,
                    wave.quiz.mode == QuizMode::FULL ? "      BOSS APPROACH CHECK        "
                                                     : "         APPROACH CHECK          ",
                    COL_CYAN, true);
    r.printCentered(3, "=================================", COL_CYAN, true);

    r.print(5, 2, wave.name, COL_YELLOW, true);
    r.printWrapped(7, 2, promptText(wave), r.cols() - 4, COL_WHITE);
    r.printWrapped(11, 2, "Examples: " + wave.examples, r.cols() - 4, COL_CYAN);

    r.print(15, 2,
            "Question " + std::to_string(state.current_quiz_question + 1) + "/" +
                std::to_string((int)wave.quiz.questions.size()),
            COL_WHITE, true);
    r.printWrapped(17, 2, question.prompt, r.cols() - 4, COL_WHITE);

    int row = 20;
    for (int i = 0; i < (int)question.options.size(); i++) {
        const std::string& option = question.options[i];
        if (i == state.selected_quiz_option) {
            r.printWrapped(row, 4, "> " + option, r.cols() - 8, COL_YELLOW);
        } else {
            r.printWrapped(row, 4, "  " + option, r.cols() - 8, COL_GRAY);
        }
        row += 2;
    }

    if (!state.quiz_feedback.empty()) {
        int color = state.last_quiz_answer_correct ? COL_GREEN : COL_RED;
        r.printWrapped(r.rows() - 4, 2, state.quiz_feedback, r.cols() - 4, color);
    }

    r.printCentered(r.rows() - 2,
                    "[UP/DOWN] Select    [ENTER] Confirm    [Q] Quit",
                    COL_YELLOW, true);
}
