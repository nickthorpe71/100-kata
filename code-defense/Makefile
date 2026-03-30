CXX = g++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra
LDFLAGS = -lncurses

SRCS = $(wildcard src/*.cpp) $(wildcard src/screens/*.cpp)
OBJS = $(SRCS:.cpp=.o)

code-defense: $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) code-defense

.PHONY: clean
