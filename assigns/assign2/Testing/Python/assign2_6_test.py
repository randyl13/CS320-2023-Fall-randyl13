####################################################
#!/usr/bin/env python3
####################################################
import sys
####################################################
sys.setrecursionlimit(16000)
####################################################
sys.path.append('./../../MySolution/Python')
sys.path.append("./../../../../classlib/Python")
####################################################
from MyPython import *
from assign2_6 import *
####################################################
print(string_merge("135", "2468"))
assert(string_merge("135", "2468") == "1234568")
assert(string_merge("abcde", "1234") == "1234abcde")
assert(string_merge("12345", "abcd") == "12345abcd")
####################################################
print("Assign2-6-test passed!")
####################################################
