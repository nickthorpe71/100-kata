#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <algorithm>

using namespace std;

class Mix
{
public:
    static string mix(const string &s1, const string &s2);
};

string Mix::mix(const string &s1, const string &s2)
{
    unordered_map<char, int> count1, count2;

    // Count occurrences of each character in both strings
    for (char c : s1)
    {
        if (islower(c))
            count1[c]++;
    }
    for (char c : s2)
    {
        if (islower(c))
            count2[c]++;
    }

    // Prepare a vector to hold the results
    vector<string> results;

    for (char c = 'a'; c <= 'z'; c++)
    {
        int maxCount = max(count1[c], count2[c]);
        if (maxCount > 1)
        {
            string prefix = count1[c] > count2[c] ? "1:" : (count1[c] < count2[c] ? "2:" : "=:");
            results.push_back(prefix + string(maxCount, c));
        }
    }

    // Sort the results: longer strings first, then lexicographically
    sort(results.begin(), results.end(), [](const string &a, const string &b)
         {
        if (a.size() != b.size())
            return a.size() > b.size();
        return a < b; });

    // Combine the results into a single string
    string result = "";
    for (size_t i = 0; i < results.size(); i++)
    {
        if (i > 0)
            result += "/";
        result += results[i];
    }

    return result;
}

int main()
{
    string s1 = "Are the kids at home? aaaaa fffff";
    string s2 = "Yes they are here! aaaaa fffff";
    cout << Mix::mix(s1, s2) << endl;

    return 0;
}