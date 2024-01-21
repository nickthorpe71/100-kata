#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <numeric>

#include <iostream>

using namespace std;

struct Weight
{
    string original;
    int value;
};

vector<string> split(const string &s, char delimiter);
int sumDigitsInString(const string &str);

class WeightSort
{
public:
    static string orderWeight(const string &strng);
};

string WeightSort::orderWeight(const string &strng)
{
    if (strng.empty())
    {
        return "";
    }

    vector<string> nums = split(strng, ' ');
    vector<Weight> weights;

    transform(nums.begin(), nums.end(), back_inserter(weights), [](const string &s)
              { return Weight{s, sumDigitsInString(s)}; });

    sort(weights.begin(), weights.end(), [](const Weight &a, const Weight &b)
         {
        if (a.value != b.value) return a.value < b.value;
        return a.original < b.original; });

    string result = accumulate(next(weights.begin()), weights.end(), weights.begin()->original,
                               [](const string &a, const Weight &b)
                               {
                                   return a + " " + b.original;
                               });

    return result;
}

vector<string> split(const string &s, char delimiter)
{
    vector<string> tokens;
    string token;
    istringstream tokenStream(s);
    while (getline(tokenStream, token, delimiter))
    {
        tokens.push_back(move(token));
    }
    return tokens;
}

int sumDigitsInString(const string &str)
{
    int sum = 0;
    for (char c : str)
    {
        if (isdigit(c))
            sum += c - '0'; // Convert char to int and add t sum
    }
    return sum;
}

int main()
{
    string result = WeightSort::orderWeight("103 123 4444 99 2000");
    cout << result << "\n";
    return 0;
}