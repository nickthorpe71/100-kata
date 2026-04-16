#include "world.h"
#include <fstream>
#include <filesystem>

void island_post::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 1301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <queue>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Island Post — a pelican postmaster on floating islands.\n";
            out << "// Find routes. Detect loops. Deliver on time.\n\n";
            out << "int deliverMail(vector<vector<int>>& bridges, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 1301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Island Post — a pelican postmaster on floating islands.\n";
            out << "// Find routes. Detect loops. Deliver on time.\n\n";
            out << "function deliverMail(bridges, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { deliverMail };\n";
            out.close();
        }
    }
}
