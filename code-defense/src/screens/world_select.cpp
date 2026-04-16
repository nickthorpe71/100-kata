#include "world_select.h"
#include <cstdlib>
#include <ctime>
#include <algorithm>

static void seedRandom() {
    static bool seeded = false;
    if (!seeded) { srand((unsigned)time(nullptr)); seeded = true; }
}

static void startRandomDrill(GameState& state) {
    seedRandom();
    std::vector<int> pool;
    for (const auto& world : state.worlds) {
        int half = (world.wave_count + 1) / 2;
        for (int i = 0; i < half; i++) {
            pool.push_back(world.start_index + i);
        }
    }

    for (int i = (int)pool.size() - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        std::swap(pool[i], pool[j]);
    }
    int count = std::min(5, (int)pool.size());

    int start = (int)state.waves.size();
    for (int i = 0; i < count; i++) {
        WaveDef w = state.waves[pool[i]];
        w.id = 9900 + i;
        state.waves.push_back(w);
    }

    state.current_wave = start;
    state.total_waves = count;
    state.world_end_wave = start + count;
    state.lives = 5;
    state.failed_waves.clear();
    state.game_started = true;
    state.random_mode = true;
    state.mode = GameMode::RANDOM_DRILL;
    state.current_screen = Screen::WAVE_INTRO;
}

static void startGauntlet(GameState& state) {
    seedRandom();
    std::vector<int> pool;
    for (const auto& world : state.worlds) {
        pool.push_back(world.start_index + world.wave_count - 1);
    }

    for (int i = (int)pool.size() - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        std::swap(pool[i], pool[j]);
    }
    int count = std::min(5, (int)pool.size());

    int start = (int)state.waves.size();
    for (int i = 0; i < count; i++) {
        WaveDef w = state.waves[pool[i]];
        w.id = 9950 + i;
        state.waves.push_back(w);
    }

    state.current_wave = start;
    state.total_waves = count;
    state.world_end_wave = start + count;
    state.lives = 3;
    state.failed_waves.clear();
    state.game_started = true;
    state.random_mode = true;
    state.mode = GameMode::GAUNTLET;
    state.current_screen = Screen::WAVE_INTRO;
}

static void startMarathon(GameState& state) {
    state.current_wave = 0;
    state.total_waves = (int)state.waves.size();
    state.world_end_wave = state.worlds[0].start_index + state.worlds[0].wave_count;
    state.lives = 5;
    state.failed_waves.clear();
    state.game_started = true;
    state.random_mode = false;
    state.mode = GameMode::MARATHON;
    state.current_screen = Screen::WAVE_INTRO;
}

// Continue (index 0), Marathon (1), Random Drill (2), Gauntlet (3), then worlds
static int getNumModes(const GameState& state) {
    return state.has_save ? 4 : 3; // Continue only if save exists
}

void handleWorldSelectScreen(GameState& state, int key) {
    int num_modes = getNumModes(state);
    int num_items = (int)state.worlds.size() + num_modes;
    if (num_items == 0) return;

    if (key == KEY_UP || key == 'k') {
        state.selected_world = (state.selected_world - 1 + num_items) % num_items;
    } else if (key == KEY_DOWN || key == 'j') {
        state.selected_world = (state.selected_world + 1) % num_items;
    } else if (key == '\n' || key == KEY_ENTER || key == ' ') {
        int idx = state.selected_world;
        int mode_offset = 0;

        // Continue option (only present if has_save)
        if (state.has_save) {
            if (idx == 0) {
                // Resume saved game — handled externally via resumeGame
                // Set a flag the main loop can check
                state.current_screen = Screen::WAVE_INTRO; // will be caught by main
                // Inline resume
                state.current_wave = state.save.current_wave;
                state.total_waves = state.save.total_waves;
                state.world_end_wave = state.save.world_end_wave;
                state.lives = state.save.lives;
                state.failed_waves = state.save.failed_waves;
                state.mode = state.save.mode;
                state.game_started = true;
                state.random_mode = false;
                state.has_save = false;
                return;
            }
            mode_offset = 1;
        }

        int adjusted = idx - mode_offset;

        if (adjusted == 0) {
            startMarathon(state);
        } else if (adjusted == 1) {
            startRandomDrill(state);
        } else if (adjusted == 2) {
            startGauntlet(state);
        } else {
            int world_idx = adjusted - 3;
            const auto& world = state.worlds[world_idx];
            state.current_wave = world.start_index;
            state.total_waves = world.wave_count;
            state.world_end_wave = world.start_index + world.wave_count;
            state.lives = 5;
            state.failed_waves.clear();
            state.game_started = true;
            state.random_mode = false;
            state.mode = GameMode::SINGLE_WORLD;
            state.current_screen = Screen::WAVE_INTRO;
        }
    } else if (key == 'l' || key == 'L') {
        state.language = (state.language == Language::CPP) ? Language::JS : Language::CPP;
    } else if (key == 'q' || key == 27) {
        state.current_screen = Screen::TITLE;
    }
}

void drawWorldSelectScreen(GameState& state, Renderer& r) {
    r.printCentered(1, "=================================", COL_GREEN, true);
    r.printCentered(2, "        SELECT  WORLD            ", COL_GREEN, true);
    r.printCentered(3, "=================================", COL_GREEN, true);

    int num_modes = getNumModes(state);
    int num_items = (int)state.worlds.size() + num_modes;
    int header_rows = 5;
    int footer_rows = 2;
    int available = r.rows() - header_rows - footer_rows;
    int visible_items = available - 1;
    if (visible_items < 3) visible_items = 3;

    int scroll_offset = state.selected_world - visible_items / 2;
    if (scroll_offset < 0) scroll_offset = 0;
    if (scroll_offset > num_items - visible_items) scroll_offset = num_items - visible_items;
    if (scroll_offset < 0) scroll_offset = 0;

    int row = header_rows;

    if (scroll_offset > 0) {
        r.printCentered(row, "--- more above ---", COL_GRAY);
        row++;
        visible_items--;
    }

    int end_index = std::min(scroll_offset + visible_items, num_items);

    for (int idx = scroll_offset; idx < end_index && row < r.rows() - footer_rows - 1; idx++) {
        std::string label;
        std::string desc;
        int sel_color = COL_YELLOW;

        if (state.has_save && idx == 0) {
            label = "Continue: " + state.save.label;
            desc = "Resume your saved run";
            sel_color = COL_GREEN;
        } else {
            int adjusted = idx - (state.has_save ? 1 : 0);
            if (adjusted == 0) {
                label = "Marathon  (all worlds)";
                desc = "Play every world in order -- save and quit anytime";
            } else if (adjusted == 1) {
                label = "Random Drill  (5 waves)";
                desc = "5 random early problems -- pick the right tool";
            } else if (adjusted == 2) {
                label = "The Gauntlet  (5 waves)";
                desc = "5 random boss waves, 3 lives -- prove your mastery";
                sel_color = COL_RED;
            } else {
                int world_idx = adjusted - 3;
                const auto& world = state.worlds[world_idx];
                label = world.name + "  (" + std::to_string(world.wave_count) + " waves)";
                desc = world.description;
            }
        }

        if (idx == state.selected_world) {
            r.printCentered(row, "> " + label + " <", sel_color, true);
            row++;
            r.printCentered(row, desc, COL_CYAN);
            row++;
        } else {
            r.printCentered(row, "  " + label + "  ", COL_GRAY);
            row++;
        }
    }

    if (end_index < num_items) {
        r.printCentered(row, "--- more below ---", COL_GRAY);
    }

    std::string lang_label = (state.language == Language::CPP) ? "C++" : "JavaScript";
    r.printCentered(r.rows() - 2, "Language: " + lang_label, COL_CYAN);
    r.printCentered(r.rows() - 1, "[UP/DOWN] Navigate  [ENTER] Select  [L] Lang  [Q] Back", COL_GRAY);
}
