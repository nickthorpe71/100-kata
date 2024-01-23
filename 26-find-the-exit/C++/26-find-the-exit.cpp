#include <iostream>
#include <vector>
#include <string>

using namespace std;

bool dfs(vector<vector<char>>& maze, int x, int y) {
    int n = maze.size();
    if (x < 0 || y < 0 || x >= n || y >= n || maze[x][y] == 'W' || maze[x][y] == 'V')
        return false;

    if (x == n - 1 && y == n - 1) return true;

    maze[x][y] = 'V'; // Mark as visited

    // Explore all four directions
    if (dfs(maze, x + 1, y) || dfs(maze, x - 1, y) || dfs(maze, x, y + 1) || dfs(maze, x, y - 1))
        return true;

    return false;
}

bool path_finder(string maze_str) {
    vector<string> rows;
    string row;
    for (char c : maze_str) {
        if (c == '\n') {
            rows.push_back(row);
            row = "";
        } else {
            row += c;
        }
    }

    rows.push_back(row); // Add the last row

    vector<vector<char>> maze;
    for (const string& r : rows) {
        vector<char> mazeRow(r.begin(), r.end());
        maze.push_back(mazeRow);
    }

    return dfs(maze, 0, 0);
}

int main() {
    cout << boolalpha;
    cout << "Test 1: " << path_finder(".W.\n.W.\n...") << endl;
    cout << "Test 2: " << path_finder(".W.\n.W.\nW..") << endl;
    cout << "Test 3: " << path_finder("......\n......\n......\n......\n......\n......") << endl;
    cout << "Test 4: " << path_finder("......\n......\n......\n......\n.....W\n....W.") << endl;
    return 0;
}