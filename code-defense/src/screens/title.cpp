#include "title.h"

void handleTitleScreen(GameState& state, int key) {
    if (key == '\n' || key == KEY_ENTER || key == ' ') {
        state.current_screen = Screen::WORLD_SELECT;
    }
    if (key == 'q') {
        state.quit = true;
    }
}

void drawTitleScreen(GameState& /*state*/, Renderer& r) {
    int mid = r.rows() / 2 - 6;

    r.printCentered(mid,     "=================================", COL_GREEN, true);
    r.printCentered(mid + 1, "         CODE  DEFENSE           ", COL_GREEN, true);
    r.printCentered(mid + 2, "=================================", COL_GREEN, true);

    r.printCentered(mid + 5, "Survive 100 waves of coding challenges", COL_WHITE);
    r.printCentered(mid + 6, "Write C++ solutions to defeat the creeps", COL_WHITE);

    r.printCentered(mid + 9,  "Edit player/solution.cpp in your editor", COL_GRAY);
    r.printCentered(mid + 10, "Test and submit from this window", COL_GRAY);

    r.printCentered(mid + 13, "[ENTER] Start    [Q] Quit", COL_YELLOW, true);
}
