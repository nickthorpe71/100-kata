#include "world.h"
#include <fstream>
#include <filesystem>

void silk_loom::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <string>\n";
            out << "#include <algorithm>\n";
            out << "#include <climits>\n";
            out << "using namespace std;\n\n";
            out << "// The Silk Loom — a silk moth laying patterns.\n";
            out << "// Each cell depends on cells above and to the left.\n\n";
            out << "int weaveLoom(vector<vector<int>>& fabric, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Silk Loom — a silk moth laying patterns.\n";
            out << "// Each cell depends on cells above and to the left.\n\n";
            out << "function weaveLoom(fabric, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { weaveLoom };\n";
            out.close();
        }
    }
}
