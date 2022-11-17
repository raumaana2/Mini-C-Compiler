CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option -fno-exceptions 

SRC = mccomp.cpp lexer.cpp parser.cpp to_string.cpp codegen.cpp

mccomp: mccomp.cpp
	$(CXX) $(SRC) $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp 
