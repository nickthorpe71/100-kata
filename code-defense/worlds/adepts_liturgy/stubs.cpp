#include "world.h"
#include <fstream>
#include <filesystem>

void adepts_liturgy::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <string>\n";
            out << "#include <algorithm>\n";
            out << "using namespace std;\n\n";
            out << "// The Adept's Liturgy — a Tech-Priest restoring corrupted machine code.\n";
            out << "// The Omnissiah's litany must be made whole. Trace each glyph. Rewrite.\n\n";
            out << "int restoreLiturgy(string& codex, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n";
            out.close();
        }

        // Wave 5 adds the canon parameter
        if (wave.id == 205) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "int restoreLiturgy(string& codex, int target)";
            std::string new_sig = "int restoreLiturgy(string& codex, string& canon, int target)";
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

        if (wave.id == 201 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Adept's Liturgy — a Tech-Priest restoring corrupted machine code.\n";
            out << "// The Omnissiah's litany must be made whole. Trace each glyph. Rewrite.\n\n";
            out << "function restoreLiturgy(codex, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return 0;\n";
            out << "}\n\n";
            out << "module.exports = { restoreLiturgy };\n";
            out.close();
        }

        // Wave 5 adds the canon parameter
        if (wave.id == 205) {
            std::ifstream in(path);
            std::string contents((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
            in.close();

            std::string old_sig = "function restoreLiturgy(codex, target)";
            std::string new_sig = "function restoreLiturgy(codex, canon, target)";
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
