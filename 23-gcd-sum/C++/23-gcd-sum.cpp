#include <vector>
#include <iostream>

// Function to compute the GCD of two numbers using unsigned long long
unsigned long long gcd(unsigned long long a, unsigned long long b) {
    while (b != 0) {
        unsigned long long temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

// Function to compute the smallest sum as per the problem statement
unsigned long long solution(const std::vector<unsigned long long>& arr) {
    if (arr.empty()) return 0; // Handle empty array case

    unsigned long long result = arr[0];
    for (std::vector<unsigned long long>::size_type i = 1; i < arr.size(); ++i) {
        result = gcd(result, arr[i]);
    }

    // The smallest possible sum is the GCD multiplied by the number of elements
    return result * arr.size();
}

int main() {
    std::vector<unsigned long long> arr = {6, 9, 21};
    std::cout << "Smallest possible sum: " << solution(arr) << std::endl;
    return 0;
}