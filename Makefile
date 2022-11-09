CXX=clang++ -std=c++17
CFLAGS= -g -O3 `llvm-config --cppflags --ldflags --system-libs --libs all` \
-Wno-unused-function -Wno-unknown-warning-option -fno-exceptions -fno-rtti -frtti



mccomp: mccomp.cpp 
	$(CXX) mccomp.cpp  $(CFLAGS) -o mccomp

clean:
	rm -rf mccomp 
