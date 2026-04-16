#include "world.h"
#include <fstream>
#include <filesystem>

void arborist::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Arborist — deciding which branches to prune.\n";
            out << "// Each cut affects the whole tree.\n";
            out << "// Tree is level-order array: children of index i at 2i+1 and 2i+2. -1 = null.\n\n";
            out << "int pruneTree(vector<int>& branches, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Arborist — deciding which branches to prune.\n";
            out << "// Each cut affects the whole tree.\n";
            out << "// Tree is level-order array: children of index i at 2i+1 and 2i+2. -1 = null.\n\n";
            out << "function pruneTree(branches, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { pruneTree };\n";
            out.close();
        }
    }
}
