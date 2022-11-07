import os
import sys

def main():
    tests = os.listdir("tests")
    tests.remove("tests.sh")

    print(tests)

    for t in tests:
        print(t)
        os.system(f"./mccomp tests/{t}/{t}.c")
    

if __name__ == "__main__": main()