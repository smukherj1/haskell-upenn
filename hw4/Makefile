TARGET = hw4
MODULE = Hw4
CXX = g++
CXXFLAGS = -Wall -O3 -march=native -mtune=native -std=c++14

$(TARGET): $(TARGET).hs
	@ghc $< -O3 -o $@ -main-is $(MODULE)

ss: ss.cpp
	$(CXX) ss.cpp -o $@ $(CXXFLAGS)