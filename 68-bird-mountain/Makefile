CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -Wpedantic -O2
TARGET = solution

all: $(TARGET)

$(TARGET): main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)

.PHONY: all run clean