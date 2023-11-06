import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def string_length (cs):
  return len(cs)

def string_get_at (cs, i):
  return cs[i]

def string_tabulate (i, func):
  ans = ""
  for x in range(i):
    ans.append(func(x))
  return ans

def string_fset_at(cs, i0, c0):
  return \
    string_tabulate\
      (string_length(cs), lambda i: string_get_at(cs,i) if i != i0 else c0)

alphabet = string_tabulate(26, lambda i: chr(ord('a') + i))

def list_make_fwork(fwork):
  res = []
  fwork(lambda x: res.append(x))
  return res

def int1_foreach(i, func):
  for x in range(i):
    func(x)

def string_foreach(cs, func):
  int1_foreach(string_length(cs))(
    lambda i0: func(string_get_at(cs)(i0))
  )

def list_of_buddies (word):
  n0 = string_length(word)
  return list_make_fwork \
    (lambda work: int1_foreach \
      (n0, lambda i0:
      string_foreach(alphabet, lambda c1: 
      work(string_fset_at(word)(i0)(c1))if c1 != string_get_at(word, i0) else None)))

