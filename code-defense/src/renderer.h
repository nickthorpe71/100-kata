#pragma once
#include <ncurses.h>
#include <string>
#include <vector>

enum Color {
    COL_WHITE = 1,
    COL_GREEN,
    COL_RED,
    COL_YELLOW,
    COL_GRAY,
    COL_CYAN
};

struct Creep {
    int col;
    int row;
    int worth;
    bool alive;
    int death_frames;
    bool turned_red;
};

class Renderer {
public:
    bool init();
    void shutdown();

    void clear();
    void present();

    void print(int row, int col, const std::string& text, int color_pair = COL_WHITE, bool bold = false);
    void printCentered(int row, const std::string& text, int color_pair = COL_WHITE, bool bold = false);
    void printWrapped(int row, int col, const std::string& text, int max_width, int color_pair = COL_WHITE);
    void drawBox(int row, int col, int h, int w, int color_pair = COL_WHITE);
    void drawHLine(int row, int col, int len, int color_pair = COL_WHITE);

    int rows() const { return max_rows; }
    int cols() const { return max_cols; }

    // Creep system
    void initCreeps(int n);
    void updateCreeps(float dt, bool correct, bool timed_out, bool wrong);
    void drawCreeps();
    bool creepAnimationDone() const;

private:
    int max_rows = 0;
    int max_cols = 0;

    std::vector<Creep> creeps;
    bool creep_anim_done = false;
    int total_creeps = 0;
    int killed_creeps = 0;
    float kill_progress = 0.0f;
    float anim_timer = 0.0f;
};
