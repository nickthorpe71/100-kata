#include "world.h"
#include <fstream>
#include <filesystem>

void clocktower::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1001 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <climits>\n";
            out << "using namespace std;\n\n";
            out << "// The Clocktower — a clockmaker disassembling impossible clocks.\n";
            out << "// Each clock contains smaller clocks. Take it apart.\n";
            out << "// The answer assembles itself on the way back up.\n\n";
            out << "int openClock(vector<int>& gears, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1001 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Clocktower — a clockmaker disassembling impossible clocks.\n";
            out << "// Each clock contains smaller clocks. Take it apart.\n";
            out << "// The answer assembles itself on the way back up.\n\n";
            out << "function openClock(gears, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { openClock };\n";
            out.close();
        }
    }
}
