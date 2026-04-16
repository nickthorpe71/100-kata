#include "world.h"
#include <fstream>
#include <filesystem>

void iron_gut::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <string>\n";
            out << "#include <vector>\n";
            out << "#include <stack>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Iron Gut — a colossal iron worm coiled beneath a dying city.\n";
            out << "// What enters its mouth last exits first. Keep it flowing.\n\n";
            out << "int feedTheGut(string& waste, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Iron Gut — a colossal iron worm coiled beneath a dying city.\n";
            out << "// What enters its mouth last exits first. Keep it flowing.\n\n";
            out << "function feedTheGut(waste, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { feedTheGut };\n";
            out.close();
        }
    }
}
