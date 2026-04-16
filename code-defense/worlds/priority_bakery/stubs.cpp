#include "world.h"
#include <fstream>
#include <filesystem>

void priority_bakery::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <queue>\n";
            out << "#include <algorithm>\n";
            out << "#include <functional>\n";
            out << "using namespace std;\n\n";
            out << "// The Priority Bakery — an octopus baker.\n";
            out << "// Always bakes the highest-priority order next.\n\n";
            out << "int bakePriority(vector<int>& orders, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Priority Bakery — an octopus baker.\n";
            out << "// Always bakes the highest-priority order next.\n\n";
            out << "function bakePriority(orders, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { bakePriority };\n";
            out.close();
        }
    }
}
