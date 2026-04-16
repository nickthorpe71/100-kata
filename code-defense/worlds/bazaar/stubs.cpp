#include "world.h"
#include <fstream>
#include <filesystem>

void bazaar::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1601 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Bazaar — a trader who always takes the best deal right now.\n";
            out << "// No second-guessing. No going back.\n\n";
            out << "int tradeBazaar(vector<int>& deals, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1601 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Bazaar — a trader who always takes the best deal right now.\n";
            out << "// No second-guessing. No going back.\n\n";
            out << "function tradeBazaar(deals, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { tradeBazaar };\n";
            out.close();
        }
    }
}
