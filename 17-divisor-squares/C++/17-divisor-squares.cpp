#include <utility>
#include <vector>
#include <iostream>
#include <cmath>

class SumSquaredDivisors
{
public:
    static std::vector<std::pair<long long, long long>> listSquared(long long m, long long n)
    {
        std::vector<std::pair<long long, long long>> results;

        for (long long i = m; i <= n; ++i) 
        {
            long long sum = sumOfSquaredDivisors(i);
            if (isSquare(sum))
            {
                results.push_back(std::make_pair(i, sum));
            }
        }

        return results;
    }

private:
    static std::vector<long long> getDivisors(long long n)
    {
        std::vector<long long> divisors;
        for (long long i = 1; i <= std::sqrt(n); ++i)
        {
            if (n % i == 0)
            {
                divisors.push_back(i);
                if (i != n / i)
                {
                    divisors.push_back(n / i);
                }
            }
        }
        return divisors;
    }

    static long long sumOfSquaredDivisors(long long n)
    {
        std::vector<long long> divisors = getDivisors(n);
        long long sum = 0;
        for (auto &divisor : divisors)
        {
            sum += divisor * divisor;
        }
        return sum;
    }

    static bool isSquare(long long n)
    {
        long long sqrtN = std::sqrt(n);
        return sqrtN * sqrtN == n;
    }
};

int main()
{
    std::vector<std::pair<long long, long long>> r = SumSquaredDivisors::listSquared(42, 250);
    for (auto &i : r)
    {
        std::cout << i.first << ", " << i.second << "\n";
    }

    return 0;
}