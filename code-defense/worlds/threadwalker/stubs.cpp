#include "world.h"
#include <fstream>
#include <filesystem>

void threadwalker::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        // Only generate the stub for wave 1 or if the file doesn't exist
        if (wave.id == 101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Threadwalker — a two-legged spider on a thread over the abyss.\n";
            out << "// The thread hums with tension values. Walk it. Survive.\n\n";
            out << "int walkThread(vector<int>& thread, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
        // For all other waves: do nothing. The player modifies their existing solution.
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 101 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Threadwalker — a two-legged spider on a thread over the abyss.\n";
            out << "// The thread hums with tension values. Walk it. Survive.\n\n";
            out << "function walkThread(thread, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { walkThread };\n";
            out.close();
        }
    }
}
