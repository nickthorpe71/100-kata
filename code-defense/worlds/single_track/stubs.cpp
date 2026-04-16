#include "world.h"
#include <fstream>
#include <filesystem>

void single_track::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1701 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Single Track — one track, many trains.\n";
            out << "// Merge schedules. Count platforms. Manage chaos.\n\n";
            out << "int dispatch(vector<vector<int>>& schedule, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1701 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Single Track — one track, many trains.\n";
            out << "// Merge schedules. Count platforms. Manage chaos.\n\n";
            out << "function dispatch(schedule, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { dispatch };\n";
            out.close();
        }
    }
}
