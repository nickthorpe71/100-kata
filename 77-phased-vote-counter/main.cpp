#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

class VoteCounter {
public:
    void addVote(const std::string& candidate) {
        throw std::logic_error("Not implemented");
    }

    void removeVote(const std::string& candidate) {
        throw std::logic_error("Not implemented");
    }

    std::string topCandidate() const {
        throw std::logic_error("Not implemented");
    }

    std::vector<std::string> topK(int k) const {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement VoteCounter phase by phase for kata 77." << std::endl;
    return 0;
}
