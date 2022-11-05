CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cxxflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option 

SRC = ast_nodes.cpp code_gen.cpp lexer.cpp mccomp.cpp parser.cpp recursive_descent.hpp

mccomp: mccomp.cpp 
	$(CXX) $(SRC) $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp 
