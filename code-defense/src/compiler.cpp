#include "compiler.h"
#include <cstdio>
#include <cstdlib>
#include <array>
#include <string>

static std::string exec(const std::string& cmd, int& exit_code) {
    std::string result;
    std::array<char, 256> buffer;

    std::string full_cmd = cmd + " 2>&1";
    FILE* pipe = popen(full_cmd.c_str(), "r");
    if (!pipe) {
        exit_code = -1;
        return "Failed to run command";
    }
    while (fgets(buffer.data(), buffer.size(), pipe) != nullptr) {
        result += buffer.data();
    }
    int status = pclose(pipe);
    exit_code = WEXITSTATUS(status);
    return result;
}

static std::string exec_split(const std::string& cmd, int& exit_code, const std::string& stderr_file) {
    // Run cmd with stderr going to a file, stdout captured
    std::string full_cmd = cmd + " 2>" + stderr_file;
    std::string result;
    std::array<char, 256> buffer;
    FILE* pipe = popen(full_cmd.c_str(), "r");
    if (!pipe) { exit_code = -1; return ""; }
    while (fgets(buffer.data(), buffer.size(), pipe) != nullptr)
        result += buffer.data();
    int status = pclose(pipe);
    exit_code = WEXITSTATUS(status);
    return result;
}

ExecutionResult compileAndRun(const std::string& player_dir, bool test_only, int time_limit_ms) {
    ExecutionResult result = {};

    std::string solution = player_dir + "/solution.cpp";
    std::string runner = player_dir + "/runner.cpp";
    std::string binary = player_dir + "/player_solution";
    std::string errfile = player_dir + "/stderr.txt";

    // Remove old binary so a stale one can't be reused
    std::remove(binary.c_str());

    // Compile (capture stderr for error messages)
    std::string compile_cmd = "g++ -std=c++17 -O2 -o " + binary + " " + solution + " " + runner;
    int exit_code = 0;
    std::string output = exec(compile_cmd, exit_code);

    if (exit_code != 0) {
        result.compiled = false;
        result.correct = false;
        result.error_msg = output;
        return result;
    }
    result.compiled = true;

    // Run with stderr separated so it can't pollute our parsed stdout
    std::string mode = test_only ? "--test" : "--full";
    int timeout_s = (time_limit_ms / 1000) + 2;
    std::string run_cmd = "timeout " + std::to_string(timeout_s) + " " + binary + " " + mode;
    output = exec_split(run_cmd, exit_code, errfile);

    if (exit_code == 124) {
        result.correct = false;
        result.time_ms = time_limit_ms;
        result.error_msg = "Time limit exceeded";
        return result;
    }

    if (exit_code > 1) {
        result.correct = false;
        result.error_msg = "Runtime error (exit code " + std::to_string(exit_code) + ")";
        return result;
    }

    // Parse: "time_ms ops\n"
    result.correct = (exit_code == 0);
    if (std::sscanf(output.c_str(), "%d %d", &result.time_ms, &result.operations) != 2) {
        result.correct = false;
        result.error_msg = "Bad output: " + output;
        return result;
    }
    if (!result.correct) {
        result.error_msg = "Wrong answer";
    }

    return result;
}
