import os
import sys

def main():
    tests = os.listdir("tests")
    tests.remove("tests.sh")


    for t in tests:
        os.system(f"./mccomp tests/{t}/{t}.c")
    

if __name__ == "__main__": main()