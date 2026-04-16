#include "world.h"
#include <fstream>
#include <filesystem>

void demolition_crew::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <string>\n";
            out << "#include <vector>\n";
            out << "#include <stack>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Demolition Crew — a demolition expert with a detonator and a plan.\n";
            out << "// Process left to right.\n\n";
            out << "int demolish(string& skyline, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Demolition Crew — a demolition expert with a detonator and a plan.\n";
            out << "// Process left to right.\n\n";
            out << "function demolish(skyline, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { demolish };\n";
            out.close();
        }
    }
}
