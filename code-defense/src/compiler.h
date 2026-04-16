#pragma once
#include "game.h"
#include <string>

ExecutionResult compileAndRun(const std::string& player_dir, bool test_only, int time_limit_ms, Language lang);
