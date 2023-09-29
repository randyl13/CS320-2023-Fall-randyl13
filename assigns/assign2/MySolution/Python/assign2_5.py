#use "./../../assign2.ml";;
#use "./../../../../classlib/Python/MyPython.py";;

import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def fnlist_make_fwork(func):
    result = fnlist_nil()
    def work(x):
        nonlocal result
        result = fnlist_cons(x, result)
    func(work)
    return fnlist_reverse(result)

