#include <iostream>
#include <stdexcept>
#include <vector>

class SocialFeed {
public:
    void post(int userId, int postId) {
        throw std::logic_error("Not implemented");
    }

    void deletePost(int postId) {
        throw std::logic_error("Not implemented");
    }

    void follow(int followerId, int followeeId) {
        throw std::logic_error("Not implemented");
    }

    void unfollow(int followerId, int followeeId) {
        throw std::logic_error("Not implemented");
    }

    std::vector<int> getFeed(int userId) const {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement SocialFeed for kata 98 in phases." << std::endl;
    return 0;
}
