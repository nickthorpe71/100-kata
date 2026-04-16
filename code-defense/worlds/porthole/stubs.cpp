#include "world.h"
#include <fstream>
#include <filesystem>

void porthole::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 801 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <deque>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Porthole — an underwater lighthouse.\n";
            out << "// A diver bolted to the floor watches the current through a porthole.\n";
            out << "// Things drift past. The porthole groans.\n\n";
            out << "int watchTheFlow(vector<int>& current, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 801 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Porthole — an underwater lighthouse.\n";
            out << "// A diver bolted to the floor watches the current through a porthole.\n";
            out << "// Things drift past. The porthole groans.\n\n";
            out << "function watchTheFlow(current, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { watchTheFlow };\n";
            out.close();
        }
    }
}
