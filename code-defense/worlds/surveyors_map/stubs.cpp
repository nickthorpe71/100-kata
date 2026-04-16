#include "world.h"
#include <fstream>
#include <filesystem>

void surveyors_map::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Surveyor's Map — charting an archipelago after an earthquake.\n";
            out << "// Which islands are still connected?\n\n";
            out << "int surveyLand(vector<vector<int>>& connections, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Surveyor's Map — charting an archipelago after an earthquake.\n";
            out << "// Which islands are still connected?\n\n";
            out << "function surveyLand(connections, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { surveyLand };\n";
            out.close();
        }
    }
}
