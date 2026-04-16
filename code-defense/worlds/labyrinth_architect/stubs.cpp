#include "world.h"
#include <fstream>
#include <filesystem>

void labyrinth_architect::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1801 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Labyrinth Architect — place walls, test paths, rip them out when they don't work.\n\n";
            out << "int buildLabyrinth(vector<int>& blueprint, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1801 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Labyrinth Architect — place walls, test paths, rip them out when they don't work.\n\n";
            out << "function buildLabyrinth(blueprint, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { buildLabyrinth };\n";
            out.close();
        }
    }
}
