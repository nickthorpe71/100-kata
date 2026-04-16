#include "world.h"
#include <fstream>
#include <filesystem>

void closing_jaws::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Closing Jaws — conjoined psychic jackals in a collapsing corridor.\n";
            out << "// One jaw from the left, one from the right. Converge before it crushes you.\n\n";
            out << "int closeJaws(vector<int>& corridor, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }

        // Wave 6 adds the corridor2 parameter
        if (wave.id == 306) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "int closeJaws(vector<int>& corridor, int target)";
            std::string new_sig = "int closeJaws(vector<int>& corridor, vector<int>& corridor2, int target)";
            auto pos = contents.find(old_sig);
            if (pos != std::string::npos) {
                contents.replace(pos, old_sig.size(), new_sig);
                std::ofstream out(path);
                out << contents;
                out.close();
            }
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 301 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Closing Jaws — conjoined psychic jackals in a collapsing corridor.\n";
            out << "// One jaw from the left, one from the right. Converge before it crushes you.\n\n";
            out << "function closeJaws(corridor, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { closeJaws };\n";
            out.close();
        }

        // Wave 6 adds the corridor2 parameter
        if (wave.id == 306) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "function closeJaws(corridor, target)";
            std::string new_sig = "function closeJaws(corridor, corridor2, target)";
            auto pos = contents.find(old_sig);
            if (pos != std::string::npos) {
                contents.replace(pos, old_sig.size(), new_sig);
                std::ofstream out(path);
                out << contents;
                out.close();
            }
        }
    }
}
