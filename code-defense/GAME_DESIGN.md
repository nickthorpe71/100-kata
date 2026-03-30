# Code Defense - Game Design

## Context

Turn DS&A practice into a tower-defense-inspired game. The player faces 100 waves of coding problems with increasing difficulty. Each wave, the game generates a `solution.cpp` with a function stub. The player writes their solution in their regular external editor. The game compiles, runs it against test cases, and visualizes N as "creeps" being processed. Fail a wave = lose a life. Lose 5 lives = game over. At the end, you see which problems you failed so you can study them.

The 100-wave list is designed so that beating all 100 means you've covered every core DS&A skill for interviews and competitive programming.

## Tech

- **C++17** with **SDL2** for the game window
- **g++** for compiling player solutions
- Player edits `solution.cpp` in their own editor (VS Code, vim, etc.)
- Game window shows: problem description, timer, lives, wave animation

## Project Structure

```
code-defense/
├── Makefile                    # Build the game executable
├── src/
│   ├── main.cpp                # Entry point, game loop
│   ├── game.h / game.cpp       # Core game state (lives, current wave, scores)
│   ├── renderer.h / renderer.cpp   # SDL2 window, text, shapes, animation
│   ├── wave.h / wave.cpp       # Wave definitions, test case generation
│   ├── compiler.h / compiler.cpp   # Compile & run player solution.cpp
│   ├── timer.h / timer.cpp     # Write-time clock (pauses during compile)
│   └── screens/
│       ├── title.h / title.cpp         # Title screen
│       ├── wave_intro.h / wave_intro.cpp   # "Wave X incoming" + problem display
│       ├── coding.h / coding.cpp       # Coding phase (timer, problem, test button)
│       ├── battle.h / battle.cpp       # Creep animation during execution
│       └── results.h / results.cpp     # Post-wave results + next wave preview
├── waves/
│   ├── wave_list.json          # The 100-wave definitions
│   └── templates/              # Function stub templates per wave
│       ├── wave_001.cpp        # e.g. int twoSum(vector<int>& nums, int target)
│       ├── wave_002.cpp
│       └── ...
├── player/
│   ├── solution.cpp            # Generated each wave - player edits this
│   └── runner.cpp              # Generated harness that calls solution + reports results
└── assets/
    └── fonts/                  # A monospace font for rendering text
```

## Game Flow

### Screens

1. **Title Screen**: "CODE DEFENSE" + Start / Continue / High Scores
2. **Wave Intro**: "WAVE 7 INCOMING" + problem name, short description, N value, time limit, write time. [START]
3. **Coding Phase**: Problem description with examples + constraints on screen. Timer counting down. Lives shown. Buttons: [Test] (compile + run small test, pauses timer), [Submit] (compile + run full test suite)
4. **Battle Animation**: Creeps (representing N) march across screen. Each creep has a "worth" (N / num_creeps_on_screen). As your code processes elements, creeps die. If execution times out or answer is wrong, remaining creeps get through. Life lost.
5. **Results**: Correct/wrong, execution time, operations count. Preview of next wave. Failed problems go to the study list.
6. **Game Over**: Final score (waves survived), list of failed problems for studying.

### Timer Rules
- Write clock counts only while player is writing (active during coding phase)
- Pauses during [Test] compilation and execution
- Pauses during [Submit] and battle animation
- Running out of write time = forced submit of whatever's in solution.cpp

### Compilation & Execution
- Game writes `solution.cpp` (stub) + `runner.cpp` (test harness) to `player/`
- Player edits `solution.cpp` in their editor
- On [Test]: `g++ -std=c++17 -O2 solution.cpp runner.cpp -o player_solution && ./player_solution --test`
- On [Submit]: `g++ -std=c++17 -O2 solution.cpp runner.cpp -o player_solution && timeout <limit> ./player_solution --full`
- Runner outputs JSON: `{ "correct": true, "time_ms": 142, "operations": 12400 }`
- Game reads the JSON to determine pass/fail and drive the animation

### runner.cpp Generation
Each wave generates a runner that:
- Includes the player's solution header
- Calls their function with test inputs
- Compares output to expected
- Measures execution time
- Outputs results as JSON to stdout

### Creep Visualization
- Max ~100 creep sprites on screen at any time
- Each creep labeled with its "worth" (N / visible_creeps)
- Creeps march left to right
- As code processes elements, creeps explode/disappear from the left
- If time limit exceeded: remaining creeps charge through → life lost animation
- If wrong answer: all creeps turn red → life lost animation

## Build & Dependencies

```makefile
CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
SDL_FLAGS = $(shell sdl2-config --cflags --libs) -lSDL2_ttf

SRCS = $(wildcard src/*.cpp) $(wildcard src/screens/*.cpp)
OBJS = $(SRCS:.cpp=.o)

code-defense: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(SDL_FLAGS)

clean:
	rm -f $(OBJS) code-defense
```

**Dependencies**: SDL2, SDL2_ttf (for text rendering). Install via:
```bash
sudo apt-get install libsdl2-dev libsdl2-ttf-dev
```
