#include <iostream>
#include <stdexcept>
#include <string>

class RateLimiter {
public:
    bool allow(const std::string& userId, int timestamp) {
        throw std::logic_error("Not implemented");
    }

    void setLimit(const std::string& userId, int limit) {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement RateLimiter for kata 80 and extend it phase by phase." << std::endl;
    return 0;
}
