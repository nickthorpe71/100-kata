#include <iostream>
#include <vector>
#include <unordered_map>

int score(const std::vector<int> &dice)
{
    std::unordered_map<int, int> counts;
    int totalScore = 0;

    // Count the frequency of each die value
    for (int die : dice)
    {
        counts[die]++;
    }

    // Calculate the score based on the rules
    for (const auto &pair : counts)
    {
        int dieValue = pair.first;
        int count = pair.second;

        if (dieValue == 1)
        {
            if (count >= 3)
            {
                totalScore += 1000; // Three 1's
                count -= 3;
            }
            totalScore += count * 100; // Each additional 1
        }
        else if (dieValue == 5)
        {
            if (count >= 3)
            {
                totalScore += 500; // Three 5's
                count -= 3;
            }
            totalScore += count * 50; // Each additional 5
        }
        else
        {
            if (count >= 3)
            {
                totalScore += dieValue * 100; // Three of 2, 3, 4, or 6
            }
        }
    }

    return totalScore;
}

int main()
{
    std::vector<int> dice1 = {5, 1, 3, 4, 1};
    std::vector<int> dice2 = {1, 1, 1, 3, 1};
    std::vector<int> dice3 = {2, 4, 4, 5, 4};

    std::cout << "Score for [5, 1, 3, 4, 1]: " << score(dice1) << std::endl;
    std::cout << "Score for [1, 1, 1, 3, 1]: " << score(dice2) << std::endl;
    std::cout << "Score for [2, 4, 4, 5, 4]: " << score(dice3) << std::endl;

    return 0;
}