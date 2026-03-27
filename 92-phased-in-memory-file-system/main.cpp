#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

class FileSystem {
public:
    std::vector<std::string> ls(const std::string& path) {
        throw std::logic_error("Not implemented");
    }

    void mkdir(const std::string& path) {
        throw std::logic_error("Not implemented");
    }

    void addContentToFile(const std::string& filePath, const std::string& content) {
        throw std::logic_error("Not implemented");
    }

    std::string readContentFromFile(const std::string& filePath) {
        throw std::logic_error("Not implemented");
    }

    void move(const std::string& sourcePath, const std::string& destinationPath) {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement FileSystem for kata 92 in phases." << std::endl;
    return 0;
}
