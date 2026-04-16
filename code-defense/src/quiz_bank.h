#pragma once
#include "game.h"
#include <algorithm>
#include <cctype>
#include <string>
#include <vector>

namespace quizbank {

inline std::string toLower(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) {
        return (char)std::tolower(c);
    });
    return text;
}

inline bool contains(const std::string& haystack, const std::string& needle) {
    return haystack.find(needle) != std::string::npos;
}

inline std::string inferTimeComplexity(const std::string& writeup) {
    const std::string lower = toLower(writeup);

    if (contains(lower, "o(n * alpha(n))") || contains(lower, "o(n*alpha(n))")) return "O(n * alpha(n))";
    if (contains(lower, "o(total * log k)")) return "O(total * log k)";
    if (contains(lower, "o(n log k)")) return "O(n log k)";
    if (contains(lower, "o(n * max_word_length)")) return "O(n * max_word_length)";
    if (contains(lower, "o(n*capacity)")) return "O(n*capacity)";
    if (contains(lower, "o(rows*cols)")) return "O(rows*cols)";
    if (contains(lower, "o(total characters)")) return "O(total chars)";
    if (contains(lower, "o(word length)")) return "O(word length)";
    if (contains(lower, "o(2^v)")) return "O(2^V)";
    if (contains(lower, "o(2^n)")) return "O(2^n)";
    if (contains(lower, "o(v+e)")) return "O(V+E)";
    if (contains(lower, "o(n+m)")) return "O(n+m)";
    if (contains(lower, "o(n log n)")) return "O(n log n)";
    if (contains(lower, "o(n*m)")) return "O(n*m)";
    if (contains(lower, "o(n^2)")) return "O(n^2)";
    if (contains(lower, "o(n * bits)")) return "O(n * bits)";
    if (contains(lower, "o(log n)")) return "O(log n)";
    if (contains(lower, "o(h)")) return "O(h)";
    if (contains(lower, "o(n)")) return "O(n)";
    if (contains(lower, "o(1)")) return "O(1)";
    return "O(n)";
}

inline std::string inferSpaceComplexity(const std::string& writeup,
                                        const std::string& default_space) {
    const std::string lower = toLower(writeup);

    if (contains(lower, "o(total chars)") || contains(lower, "o(total characters)")) {
        return "O(total chars) space";
    }
    if (contains(lower, "o(total chars) space") || contains(lower, "o(total characters) space")) {
        return "O(total chars) space";
    }
    if (contains(lower, "o(word length) space")) return "O(word length) space";
    if (contains(lower, "o(n) extra space")) return "O(n) space";
    if (contains(lower, "o(log n) stack")) return "O(log n) space";
    if (contains(lower, "o(n) depth")) return "O(n) space";
    if (contains(lower, "o(log n) depth")) return "O(log n) space";
    if (contains(lower, "o(k) for")) return "O(k) space";
    if (contains(lower, "o(k) space")) return "O(k) space";
    if (contains(lower, "o(v) space")) return "O(V) space";
    if (contains(lower, "o(h) space")) return "O(h) space";
    if (contains(lower, "o(log n) space")) return "O(log n) space";
    if (contains(lower, "o(n*m) space")) return "O(n*m) space";
    if (contains(lower, "o(n) space")) return "O(n) space";
    if (contains(lower, "o(1) space")) return "O(1) space";
    return default_space;
}

inline std::string inferCombinedComplexity(const std::string& writeup,
                                           const std::string& default_space) {
    return inferTimeComplexity(writeup) + " time / " + inferSpaceComplexity(writeup, default_space);
}

inline std::vector<std::string> allReasonOptions() {
    return {
        "Need fast lookup of prior values or counts",
        "Need ordered character-by-character scanning or in-place text updates",
        "Need direct bit-level operations to count, isolate, or cancel state",
        "Need ordered or monotonic structure so each comparison discards half the search space",
        "Need prefix-based branching over characters for fast lookup and prefix aggregation",
        "Need self-similar subproblems with clear base cases and recursive transitions",
        "Need coordinated pointer movement across a sequence to avoid extra passes or storage",
        "Need to search a monotonic answer space and validate each guess with a feasibility check",
        "Need LIFO state to preserve the most recent unresolved decisions",
        "Need to maintain a contiguous window while updating counts incrementally",
        "Need sequential node traversal and careful pointer rewiring without random access",
        "Need global ordering or pivot-based partitioning to make progress efficiently",
        "Need to combine results from child subtrees into a parent decision",
        "Need locally optimal choices that preserve the global best answer",
        "Need to explore nodes and edges while tracking visited state",
        "Need to explore choices, undo them, and prune invalid partial states",
        "Need to reuse overlapping subproblems instead of recomputing them",
        "Need repeated access to the current smallest or largest frontier element",
        "Need to combine answers from neighboring subproblems in a grid or table state",
        "Need ranges in sorted order so overlaps and gaps can be processed sequentially",
        "Need FIFO frontier processing to expand level by level",
        "Need to maintain dynamic connectivity under repeated union operations",
        "Need direct index-based access for scanning, partitioning, or rearranging elements",
        "Need parent-child recursive decomposition over subtree state"
    };
}

inline std::vector<std::string> buildReasonOptions(const std::string& correct_reason) {
    std::vector<std::string> options;
    const std::vector<std::string> pool = allReasonOptions();
    size_t start = correct_reason.size() % pool.size();

    for (size_t offset = 0; offset < pool.size() && options.size() < 3; offset++) {
        const std::string& candidate = pool[(start + offset) % pool.size()];
        if (candidate != correct_reason) options.push_back(candidate);
    }

    int insert_at = (int)(correct_reason.size() % 4);
    options.insert(options.begin() + insert_at, correct_reason);
    return options;
}

inline std::vector<std::string> allComplexityOptions() {
    return {
        "O(1) time / O(1) space",
        "O(log n) time / O(1) space",
        "O(log n) time / O(log n) space",
        "O(n) time / O(1) space",
        "O(n) time / O(h) space",
        "O(n) time / O(n) space",
        "O(n) time / O(k) space",
        "O(n log k) time / O(k) space",
        "O(n log n) time / O(1) space",
        "O(n log n) time / O(n) space",
        "O(n*m) time / O(n*m) space",
        "O(n*capacity) time / O(n*capacity) space",
        "O(n * max_word_length) time / O(n) space",
        "O(n * bits) time / O(1) space",
        "O(V+E) time / O(V) space",
        "O(n * alpha(n)) time / O(n) space",
        "O(total chars) time / O(total chars) space",
        "O(word length) time / O(total chars) space",
        "O(total * log k) time / O(k) space",
        "O(2^n) time / O(n) space",
        "O(2^V) time / O(V) space",
        "O(n+m) time / O(k) space"
    };
}

inline std::vector<std::string> buildComplexityOptions(const std::string& correct_complexity) {
    std::vector<std::string> options;
    const std::vector<std::string> pool = allComplexityOptions();
    size_t start = correct_complexity.size() % pool.size();

    for (size_t offset = 0; offset < pool.size() && options.size() < 3; offset++) {
        const std::string& candidate = pool[(start + offset) % pool.size()];
        if (candidate != correct_complexity) options.push_back(candidate);
    }

    int insert_at = (int)(correct_complexity.size() % 4);
    options.insert(options.begin() + insert_at, correct_complexity);
    return options;
}

inline WaveQuiz makeThemedQuiz(const std::string& approach_label,
                               const std::string& correct_reason,
                               const std::string& correct_complexity) {
    WaveQuiz quiz;
    quiz.mode = QuizMode::THEMED_LIGHT;

    std::vector<std::string> reason_options = buildReasonOptions(correct_reason);
    int reason_correct_index = 0;
    for (int i = 0; i < (int)reason_options.size(); i++) {
        if (reason_options[i] == correct_reason) {
            reason_correct_index = i;
            break;
        }
    }

    std::vector<std::string> complexity_options = buildComplexityOptions(correct_complexity);
    int complexity_correct_index = 0;
    for (int i = 0; i < (int)complexity_options.size(); i++) {
        if (complexity_options[i] == correct_complexity) {
            complexity_correct_index = i;
            break;
        }
    }

    quiz.questions.push_back({
        "Why does " + approach_label + " fit this wave best?",
        reason_options,
        reason_correct_index
    });
    quiz.questions.push_back({
        "What is the expected complexity of the intended solution?",
        complexity_options,
        complexity_correct_index
    });
    return quiz;
}

inline WaveQuiz makeFullQuiz(const std::vector<std::string>& approach_options,
                             int correct_approach_index,
                             const std::string& correct_reason,
                             const std::string& correct_complexity) {
    WaveQuiz quiz;
    quiz.mode = QuizMode::FULL;

    std::vector<std::string> reason_options = buildReasonOptions(correct_reason);
    int reason_correct_index = 0;
    for (int i = 0; i < (int)reason_options.size(); i++) {
        if (reason_options[i] == correct_reason) {
            reason_correct_index = i;
            break;
        }
    }

    std::vector<std::string> complexity_options = buildComplexityOptions(correct_complexity);
    int complexity_correct_index = 0;
    for (int i = 0; i < (int)complexity_options.size(); i++) {
        if (complexity_options[i] == correct_complexity) {
            complexity_correct_index = i;
            break;
        }
    }

    quiz.questions.push_back({
        "Which approach family fits this boss wave best?",
        approach_options,
        correct_approach_index
    });
    quiz.questions.push_back({
        "Why does that approach fit best here?",
        reason_options,
        reason_correct_index
    });
    quiz.questions.push_back({
        "What is the expected complexity of the intended solution?",
        complexity_options,
        complexity_correct_index
    });
    return quiz;
}

inline void attachThemedQuiz(std::vector<WaveDef>& waves,
                             const std::string& approach_label,
                             const std::string& correct_reason,
                             const std::string& default_space) {
    for (auto& wave : waves) {
        wave.quiz = makeThemedQuiz(
            approach_label,
            correct_reason,
            inferCombinedComplexity(wave.writeup, default_space)
        );
    }
}

}  // namespace quizbank
