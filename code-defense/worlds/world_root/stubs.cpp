#include "world.h"
#include <fstream>
#include <filesystem>

void world_root::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The World Root — Yggdrasil, the world tree.\n";
            out << "// Every node is a realm. Every path is a fate.\n";
            out << "// Tree is level-order: children of i are at 2i+1 and 2i+2. -1 = null.\n\n";
            out << "int climbYggdrasil(vector<int>& tree, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The World Root — Yggdrasil, the world tree.\n";
            out << "// Every node is a realm. Every path is a fate.\n";
            out << "// Tree is level-order: children of i are at 2i+1 and 2i+2. -1 = null.\n\n";
            out << "function climbYggdrasil(tree, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { climbYggdrasil };\n";
            out.close();
        }
    }
}
