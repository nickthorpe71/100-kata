#include "world.h"
#include <fstream>
#include <filesystem>

void spreading_rot::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 601 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <queue>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Spreading Rot — sentient fungus consuming a buried god.\n";
            out << "// The oldest tendrils spread first. Always first in, first out.\n\n";
            out << "int spreadRot(vector<vector<int>>& grid, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 601 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Spreading Rot — sentient fungus consuming a buried god.\n";
            out << "// The oldest tendrils spread first. Always first in, first out.\n\n";
            out << "function spreadRot(grid, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { spreadRot };\n";
            out.close();
        }
    }
}
