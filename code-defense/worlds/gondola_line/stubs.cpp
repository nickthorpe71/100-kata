#include "world.h"
#include <fstream>
#include <filesystem>

void gondola_line::generateStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    if (lang == Language::CPP) {
        std::string path = player_dir + "/solution.cpp";

        if (wave.id == 701 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "#include <cstdlib>\n";
            out << "using namespace std;\n\n";
            out << "// The Gondola Line — cable cars through a vertical city on a cliff.\n";
            out << "// Ancient, temperamental. You're the sole engineer. Wrench. No budget.\n\n";
            out << "struct ListNode {\n";
            out << "    int val;\n";
            out << "    ListNode* next;\n";
            out << "    ListNode(int v) : val(v), next(nullptr) {}\n";
            out << "};\n\n";
            out << "ListNode* fixTheLine(ListNode* head, ListNode* head2, int target) {\n";
            out << "    // TODO: implement\n";
            out << "    return nullptr;\n";
            out << "}\n";
            out.close();
        }
    } else {
        std::string path = player_dir + "/solution.js";

        if (wave.id == 701 || !std::filesystem::exists(path)) {
            std::ofstream out(path);
            out << "// The Gondola Line — cable cars through a vertical city on a cliff.\n";
            out << "// Ancient, temperamental. You're the sole engineer. Wrench. No budget.\n\n";
            out << "class ListNode {\n";
            out << "    constructor(val) { this.val = val; this.next = null; }\n";
            out << "}\n\n";
            out << "function fixTheLine(head, head2, target) {\n";
            out << "    // TODO: implement\n";
            out << "    return null;\n";
            out << "}\n\n";
            out << "module.exports = { fixTheLine, ListNode };\n";
            out.close();
        }
    }
}
