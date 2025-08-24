#include <iostream>
#include <string>
#include <sstream>

std::string primeFactors(int n) {
    std::stringstream result;
    for (int i = 2; i <= n / i; i++) {
        if (n % i == 0) {
            int count = 0;
            while (n % i == 0) {
                n /= i;
                count++;
            }
            result << "(" << i;
            if (count > 1) {
                result << "**" << count;
            }
            result << ")";
        }
    }
    if (n > 1) {
        result << "(" << n << ")";
    }
    return result.str();
}

int main() {
    int n = 86240;
    std::cout << "Prime factor decomposition of " << n << ": " << primeFactors(n) << std::endl;
    return 0;
}