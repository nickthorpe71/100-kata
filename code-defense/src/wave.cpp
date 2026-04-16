#include "wave.h"
// Fundamentals
#include "../worlds/threadwalker/world.h"
#include "../worlds/adepts_liturgy/world.h"
#include "../worlds/hash_maps/world.h"
#include "../worlds/closing_jaws/world.h"
#include "../worlds/hanging_court/world.h"
// Linear Structures
#include "../worlds/iron_gut/world.h"
#include "../worlds/spreading_rot/world.h"
#include "../worlds/gondola_line/world.h"
// Windowing & Search
#include "../worlds/porthole/world.h"
#include "../worlds/buried_signal/world.h"
// Recursion & Trees
#include "../worlds/clocktower/world.h"
#include "../worlds/world_root/world.h"
// Advanced Structures
#include "../worlds/priority_bakery/world.h"
#include "../worlds/island_post/world.h"
// Optimization
#include "../worlds/moths_path/world.h"
#include "../worlds/silk_loom/world.h"
#include "../worlds/bazaar/world.h"
#include "../worlds/single_track/world.h"
// More Patterns
#include "../worlds/labyrinth_architect/world.h"
#include "../worlds/binary_telegraph/world.h"
#include "../worlds/card_catalog/world.h"
#include "../worlds/surveyors_map/world.h"
// Combination Worlds
#include "../worlds/flow_lab/world.h"
#include "../worlds/arborist/world.h"
#include "../worlds/demolition_crew/world.h"
#include "../worlds/deep_mine/world.h"

std::vector<WaveDef> loadWaves() {
    std::vector<WaveDef> all;

    // Fundamentals
    auto tw = threadwalker::loadWaves();
    all.insert(all.end(), tw.begin(), tw.end());

    auto al = adepts_liturgy::loadWaves();
    all.insert(all.end(), al.begin(), al.end());

    auto hm = hash_maps::loadWaves();
    all.insert(all.end(), hm.begin(), hm.end());

    auto cj = closing_jaws::loadWaves();
    all.insert(all.end(), cj.begin(), cj.end());

    auto hc = hanging_court::loadWaves();
    all.insert(all.end(), hc.begin(), hc.end());

    // Linear Structures
    auto ig = iron_gut::loadWaves();
    all.insert(all.end(), ig.begin(), ig.end());

    auto sr = spreading_rot::loadWaves();
    all.insert(all.end(), sr.begin(), sr.end());

    auto gl = gondola_line::loadWaves();
    all.insert(all.end(), gl.begin(), gl.end());

    // Windowing & Search
    auto ph = porthole::loadWaves();
    all.insert(all.end(), ph.begin(), ph.end());

    auto bs = buried_signal::loadWaves();
    all.insert(all.end(), bs.begin(), bs.end());

    // Recursion & Trees
    auto ct = clocktower::loadWaves();
    all.insert(all.end(), ct.begin(), ct.end());

    auto wr = world_root::loadWaves();
    all.insert(all.end(), wr.begin(), wr.end());

    // Advanced Structures
    auto pb = priority_bakery::loadWaves();
    all.insert(all.end(), pb.begin(), pb.end());

    auto ip = island_post::loadWaves();
    all.insert(all.end(), ip.begin(), ip.end());

    // Optimization
    auto mp = moths_path::loadWaves();
    all.insert(all.end(), mp.begin(), mp.end());

    auto sl = silk_loom::loadWaves();
    all.insert(all.end(), sl.begin(), sl.end());

    auto bz = bazaar::loadWaves();
    all.insert(all.end(), bz.begin(), bz.end());

    auto st = single_track::loadWaves();
    all.insert(all.end(), st.begin(), st.end());

    // More Patterns
    auto la = labyrinth_architect::loadWaves();
    all.insert(all.end(), la.begin(), la.end());

    auto bt = binary_telegraph::loadWaves();
    all.insert(all.end(), bt.begin(), bt.end());

    auto cc = card_catalog::loadWaves();
    all.insert(all.end(), cc.begin(), cc.end());

    auto sm = surveyors_map::loadWaves();
    all.insert(all.end(), sm.begin(), sm.end());

    // Combination Worlds
    auto fl = flow_lab::loadWaves();
    all.insert(all.end(), fl.begin(), fl.end());

    auto ar = arborist::loadWaves();
    all.insert(all.end(), ar.begin(), ar.end());

    auto dc = demolition_crew::loadWaves();
    all.insert(all.end(), dc.begin(), dc.end());

    auto dm = deep_mine::loadWaves();
    all.insert(all.end(), dm.begin(), dm.end());

    return all;
}

void generateSolutionStub(const WaveDef& wave, const std::string& player_dir, Language lang) {
    wave.gen_stub(wave, player_dir, lang);
}

void generateRunner(const WaveDef& wave, const std::string& player_dir, Language lang) {
    wave.gen_runner(wave, player_dir, lang);
}
