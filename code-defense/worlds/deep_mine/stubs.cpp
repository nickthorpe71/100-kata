#include "world.h"
#include <fstream>
#include <filesystem>

void deep_mine::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 2501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Deep Mine — binary search on the answer.\n";
            out << "// Too shallow and you miss ore, too deep and the shaft collapses.\n\n";
            out << "int digDeep(vector<int>& shaft, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 2501 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Deep Mine — binary search on the answer.\n";
            out << "// Too shallow and you miss ore, too deep and the shaft collapses.\n\n";
            out << "function digDeep(shaft, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { digDeep };\n";
            out.close();
        }
    }
}
