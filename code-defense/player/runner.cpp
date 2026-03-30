#include <vector>
#include <string>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
using namespace std;

vector<int> twoSum(vector<int>& nums, int target);

int main(int argc, char* argv[]) {
    bool test_only = (argc > 1 && string(argv[1]) == "--test");
    bool all_correct = true;
    int ops = 0;

    auto start = chrono::high_resolution_clock::now();

    {
        vector<int> v = {2, 7, 11, 15};
        auto r = twoSum(v, 9);
        sort(r.begin(), r.end());
        if (r.size() != 2 || r[0] != 0 || r[1] != 1) all_correct = false;
    }
    if (!test_only) {
        int n = 1000000;
        vector<int> v(n);
        // All negative except two large positives — guarantees unique pair
        for (int i = 0; i < n; i++) v[i] = -(i + 1);
        v[n/4] = 500000000;
        v[n/4*3] = 500000001;
        int target = 1000000001;
        auto r = twoSum(v, target);
        sort(r.begin(), r.end());
        if (r.size() != 2 || r[0] != n/4 || r[1] != n/4*3) all_correct = false;
        ops = n;
    } else { ops = 4; }

    auto end = chrono::high_resolution_clock::now();
    int ms = (int)chrono::duration_cast<chrono::milliseconds>(end - start).count();

    printf("%d %d\n", ms, ops);
    return all_correct ? 0 : 1;
}
