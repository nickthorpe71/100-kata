#include "world.h"
#include <fstream>
#include <filesystem>

void buried_signal::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 901 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Buried Signal — a radio telescope pointed into the earth.\n";
            out << "// Something below responds. Dial in. Halve the band. The desert hums.\n\n";
            out << "int dialIn(vector<int>& bands, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 901 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Buried Signal — a radio telescope pointed into the earth.\n";
            out << "// Something below responds. Dial in. Halve the band. The desert hums.\n\n";
            out << "function dialIn(bands, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { dialIn };\n";
            out.close();
        }
    }
}
