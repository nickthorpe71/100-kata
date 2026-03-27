#include <iostream>
#include <stdexcept>

class CalendarBooking {
public:
    bool book(int start, int end) {
        throw std::logic_error("Not implemented");
    }

    int earliestAvailable(int duration, int windowStart, int windowEnd) const {
        throw std::logic_error("Not implemented");
    }
};

int main() {
    std::cout << "Implement CalendarBooking for kata 83 in phases." << std::endl;
    return 0;
}
