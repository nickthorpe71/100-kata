#include "renderer.h"
#include <algorithm>
#include <cstring>

bool Renderer::init() {
    initscr();
    cbreak();
    noecho();
    nodelay(stdscr, TRUE);
    keypad(stdscr, TRUE);
    curs_set(0);

    if (has_colors()) {
        start_color();
        use_default_colors();
        init_pair(COL_WHITE,  COLOR_WHITE,   -1);
        init_pair(COL_GREEN,  COLOR_GREEN,   -1);
        init_pair(COL_RED,    COLOR_RED,     -1);
        init_pair(COL_YELLOW, COLOR_YELLOW,  -1);
        init_pair(COL_GRAY,   COLOR_WHITE,   -1);  // no true gray in basic ncurses
        init_pair(COL_CYAN,   COLOR_CYAN,    -1);
    }

    getmaxyx(stdscr, max_rows, max_cols);
    return true;
}

void Renderer::shutdown() {
    endwin();
}

void Renderer::clear() {
    erase();
}

void Renderer::present() {
    refresh();
}

void Renderer::print(int row, int col, const std::string& text, int color_pair, bool bold) {
    if (row < 0 || row >= max_rows || col < 0) return;
    if (bold) attron(A_BOLD);
    attron(COLOR_PAIR(color_pair));
    mvprintw(row, col, "%.*s", max_cols - col, text.c_str());
    attroff(COLOR_PAIR(color_pair));
    if (bold) attroff(A_BOLD);
}

void Renderer::printCentered(int row, const std::string& text, int color_pair, bool bold) {
    int col = (max_cols - (int)text.size()) / 2;
    if (col < 0) col = 0;
    print(row, col, text, color_pair, bold);
}

void Renderer::printWrapped(int row, int col, const std::string& text, int max_width, int color_pair) {
    int cur_row = row;
    int cur_col = col;

    // Split on newlines first, then wrap
    size_t start = 0;
    while (start < text.size() && cur_row < max_rows) {
        size_t nl = text.find('\n', start);
        std::string line = (nl == std::string::npos) ? text.substr(start) : text.substr(start, nl - start);
        start = (nl == std::string::npos) ? text.size() : nl + 1;

        // Wrap this line
        while (!line.empty() && cur_row < max_rows) {
            if ((int)line.size() <= max_width) {
                print(cur_row, cur_col, line, color_pair);
                cur_row++;
                break;
            }
            // Find last space within max_width
            int cut = max_width;
            while (cut > 0 && line[cut] != ' ') cut--;
            if (cut == 0) cut = max_width;
            print(cur_row, cur_col, line.substr(0, cut), color_pair);
            line = line.substr(cut + (line[cut] == ' ' ? 1 : 0));
            cur_row++;
        }
    }
}

void Renderer::drawBox(int row, int col, int h, int w, int color_pair) {
    attron(COLOR_PAIR(color_pair));
    // Top
    mvaddch(row, col, ACS_ULCORNER);
    for (int i = 1; i < w - 1; i++) mvaddch(row, col + i, ACS_HLINE);
    mvaddch(row, col + w - 1, ACS_URCORNER);
    // Sides
    for (int i = 1; i < h - 1; i++) {
        mvaddch(row + i, col, ACS_VLINE);
        mvaddch(row + i, col + w - 1, ACS_VLINE);
    }
    // Bottom
    mvaddch(row + h - 1, col, ACS_LLCORNER);
    for (int i = 1; i < w - 1; i++) mvaddch(row + h - 1, col + i, ACS_HLINE);
    mvaddch(row + h - 1, col + w - 1, ACS_LRCORNER);
    attroff(COLOR_PAIR(color_pair));
}

void Renderer::drawHLine(int row, int col, int len, int color_pair) {
    attron(COLOR_PAIR(color_pair));
    move(row, col);
    for (int i = 0; i < len && col + i < max_cols; i++) addch(ACS_HLINE);
    attroff(COLOR_PAIR(color_pair));
}

void Renderer::initCreeps(int n) {
    creeps.clear();
    int num_visible = std::min(n, 50);
    total_creeps = num_visible;
    killed_creeps = 0;
    kill_progress = 0.0f;
    anim_timer = 0.0f;
    creep_anim_done = false;

    int worth = (num_visible > 0) ? n / num_visible : 0;
    int field_width = std::min(max_cols - 4, 70);
    int creeps_per_row = field_width / 4;
    if (creeps_per_row < 1) creeps_per_row = 1;

    int start_row = 10;
    int start_col = 3;

    for (int i = 0; i < num_visible; i++) {
        Creep c;
        c.col = start_col + (i % creeps_per_row) * 4;
        c.row = start_row + (i / creeps_per_row);
        c.worth = worth;
        c.alive = true;
        c.death_frames = 0;
        c.turned_red = false;
        creeps.push_back(c);
    }
}

void Renderer::updateCreeps(float dt, bool correct, bool timed_out, bool wrong) {
    if (creep_anim_done) return;
    anim_timer += dt;

    if (correct) {
        // Kill creeps progressively over 2 seconds
        float progress = anim_timer / 2.0f;
        if (progress > 1.0f) progress = 1.0f;
        int to_kill = (int)(progress * total_creeps);
        for (int i = 0; i < (int)creeps.size() && i < to_kill; i++) {
            if (creeps[i].alive) {
                creeps[i].alive = false;
                creeps[i].death_frames = 3;
            }
        }
        // Tick down death animations
        for (auto& c : creeps) {
            if (!c.alive && c.death_frames > 0 && anim_timer > 0.3f) {
                c.death_frames--;
            }
        }
        bool any_alive = false;
        bool any_anim = false;
        for (auto& c : creeps) {
            if (c.alive) any_alive = true;
            if (!c.alive && c.death_frames > 0) any_anim = true;
        }
        if (!any_alive && !any_anim) creep_anim_done = true;
    } else if (wrong) {
        for (auto& c : creeps) c.turned_red = true;
        if (anim_timer > 2.0f) creep_anim_done = true;
    } else if (timed_out) {
        // March creeps right
        if ((int)(anim_timer * 5) > (int)((anim_timer - dt) * 5)) {
            for (auto& c : creeps) {
                if (c.alive) c.col += 2;
            }
        }
        if (anim_timer > 2.0f) creep_anim_done = true;
    }
}

void Renderer::drawCreeps() {
    for (auto& c : creeps) {
        if (!c.alive && c.death_frames > 0) {
            // Explosion
            print(c.row, c.col, "**", COL_YELLOW, true);
        } else if (c.alive) {
            int color = c.turned_red ? COL_RED : COL_GREEN;
            print(c.row, c.col, "<>", color, true);
        }
    }
}

bool Renderer::creepAnimationDone() const {
    return creep_anim_done;
}
