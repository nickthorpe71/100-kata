#include "world.h"
#include <fstream>
#include <filesystem>

void flow_lab::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <unordered_map>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Flow Lab — a chemist monitoring a stream through a narrow analysis window.\n";
            out << "// Track what flows through.\n\n";
            out << "int analyzeFlow(vector<int>& stream, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Flow Lab — a chemist monitoring a stream through a narrow analysis window.\n";
            out << "// Track what flows through.\n\n";
            out << "function analyzeFlow(stream, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { analyzeFlow };\n";
            out.close();
        }
    }
}
