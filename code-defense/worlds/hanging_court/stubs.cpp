#include "world.h"
#include <fstream>
#include <filesystem>

void hanging_court::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Hanging Court — a skeletal judge with scales for eyes.\n";
            out << "// Arrange the accused by guilt before the executioner arrives.\n\n";
            out << "int passJudgment(vector<int>& accused, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }

        // Wave 6 adds the docket parameter
        if (wave.id == 406) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "int passJudgment(vector<int>& accused, int target)";
            std::string new_sig = "int passJudgment(vector<int>& accused, vector<int>& docket, int target)";
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

        if (wave.id == 401 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Hanging Court — a skeletal judge with scales for eyes.\n";
            out << "// Arrange the accused by guilt before the executioner arrives.\n\n";
            out << "function passJudgment(accused, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { passJudgment };\n";
            out.close();
        }

        // Wave 6 adds the docket parameter
        if (wave.id == 406) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "function passJudgment(accused, target)";
            std::string new_sig = "function passJudgment(accused, docket, target)";
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
