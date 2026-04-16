#include <vector>
#include <string>
#include <deque>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <algorithm>
using namespace std;

int watchTheFlow(vector<int>& current, int target);

int main(int argc, char* argv[]) {
    bool test_only = (argc > 1 && string(argv[1]) == "--test");
    bool all_correct = true;
    int ops = 0;

    auto start = chrono::high_resolution_clock::now();


    auto end = chrono::high_resolution_clock::now();
    int ms = (int)chrono::duration_cast<chrono::milliseconds>(end - start).count();

    printf("%d %d\n", ms, ops);
    return all_correct ? 0 : 1;
}
