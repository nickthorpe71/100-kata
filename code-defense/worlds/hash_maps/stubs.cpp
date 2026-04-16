#include "world.h"
#include <fstream>
#include <filesystem>

void hash_maps::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        // Only generate the stub for wave 1 or if the file doesn't exist
        if (wave.id == 1 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <vector>\n";
            out << "#include <string>\n";
            out << "#include <unordered_map>\n";
            out << "#include <unordered_set>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// Hash Valley — caterpillar folk tending their hash stash.\n";
            out << "// Build your frequency map. Tend the crop. Survive the audit.\n\n";
            out << "int tendTheHash(vector<string>& stash, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }

        // Wave 4 adds the batch2 parameter — update the signature if needed
        if (wave.id == 4) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            // Replace old signature with new one that includes batch2
            std::string old_sig = "int tendTheHash(vector<string>& stash, int target)";
            std::string new_sig = "int tendTheHash(vector<string>& stash, vector<string>& batch2, int target)";
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

        if (wave.id == 1 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// Hash Valley — caterpillar folk tending their hash stash.\n";
            out << "// Build your frequency map. Tend the crop. Survive the audit.\n\n";
            out << "function tendTheHash(stash, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { tendTheHash };\n";
            out.close();
        }

        // Wave 4 adds the batch2 parameter
        if (wave.id == 4) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "function tendTheHash(stash, target)";
            std::string new_sig = "function tendTheHash(stash, batch2, target)";
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
