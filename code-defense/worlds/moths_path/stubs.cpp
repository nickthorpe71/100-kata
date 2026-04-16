#include "world.h"
#include <fstream>
#include <filesystem>

void moths_path::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "#include <climits>\n";
            out << "using namespace std;\n\n";
            out << "// The Moth's Path — a moth navigating lanterns in a night market.\n";
            out << "// Always forward. Choices can't be undone.\n\n";
            out << "int flutterForward(vector<int>& lanterns, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Moth's Path — a moth navigating lanterns in a night market.\n";
            out << "// Always forward. Choices can't be undone.\n\n";
            out << "function flutterForward(lanterns, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { flutterForward };\n";
            out.close();
        }
    }
}
