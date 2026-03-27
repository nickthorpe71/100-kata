#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

class AutocompleteSystem {
public:
    AutocompleteSystem(const std::vector<std::string>& sentences, const std::vector<int>& times) {
        throw std::logic_error("Not implemented");
    }

    std::vector<std::string> input(char c) {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement AutocompleteSystem for kata 100 and add interactive tests." << std::endl;
    return 0;
}
