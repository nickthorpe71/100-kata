#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

class SearchSuggester {
public:
    explicit SearchSuggester(const std::vector<std::string>& products) {
        throw std::logic_error("Not implemented");
    }

    void addProduct(const std::string& product) {
        throw std::logic_error("Not implemented");
    }

    std::vector<std::vector<std::string>> suggest(const std::string& searchWord) const {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement SearchSuggester for kata 86 in phases." << std::endl;
    return 0;
}
