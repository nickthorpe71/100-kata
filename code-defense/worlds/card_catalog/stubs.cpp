#include "world.h"
#include <fstream>
#include <filesystem>

void card_catalog::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2001 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <string>\n";
            out << "#include <unordered_map>\n";
            out << "using namespace std;\n\n";
            out << "// The Card Catalog — a vast library.\n";
            out << "// Books filed letter by letter in a branching card catalog.\n\n";
            out << "int searchCatalog(vector<string>& titles, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2001 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Card Catalog — a vast library.\n";
            out << "// Books filed letter by letter in a branching card catalog.\n\n";
            out << "function searchCatalog(titles, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { searchCatalog };\n";
            out.close();
        }
    }
}
