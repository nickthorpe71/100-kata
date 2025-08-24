#include <vector>
#include <cstdlib>
#include <string>
#include <iostream>

using namespace std;

vector<vector<int>> pyramid(size_t n)
{
    vector<vector<int>> result;

    for (size_t i = 0; i < n; ++i)
    {
        vector<int> row;
        for (size_t j = 0; j <= i; ++j)
        {
            row.push_back(1);
        }
        result.push_back(row);
    }

    return result;
}

int main()
{
    vector<vector<int>> result = pyramid(10);

    for (size_t i = 0; i < result.size(); ++i)
    {
        for (size_t j = 0; j < result[i].size(); ++j)
        {
            cout << result[i][j] << " ";
        }
        cout << endl;
    }

    return 0;
}