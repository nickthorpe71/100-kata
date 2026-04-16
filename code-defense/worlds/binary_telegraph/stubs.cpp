#include "world.h"
#include <fstream>
#include <filesystem>

void binary_telegraph::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1901 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "using namespace std;\n\n";
            out << "// The Binary Telegraph — everything is on or off. The truth is in the bits.\n\n";
            out << "int tapTheWire(vector<int>& signals, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1901 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Binary Telegraph — everything is on or off. The truth is in the bits.\n\n";
            out << "function tapTheWire(signals, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { tapTheWire };\n";
            out.close();
        }
    }
}
