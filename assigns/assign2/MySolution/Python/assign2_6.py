
import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def string_length(cs):
  return len(cs)

def string_get_at(cs, index):
  return cs[index]

def string_make_fwork (func):
  result = ""
  def work(x):
    return result + x
  
  return work(func)

def string_merge (cs1, cs2):
  n1 = string_length(cs1)
  n2 = string_length(cs2)
  
  def foreach(i1, i2):
    while i1 < n1 or i2 < n2:
      if i1 < n1:
        if i2 < n2:
          c1 = string_get_at(cs1, i1)
          c2 = string_get_at(cs2, i2)
          if c1 <= c2:
            return c1 + foreach(i1+1, i2+0)
          else: 
            return c2 + foreach(i1+0, i2+1)
        else:
          c1 = string_get_at(cs1, i1)
          return c1 + foreach(i1+1, i2+0)
      else:
        c2 = string_get_at(cs2, i2)
        return  c2 + foreach(i1+0, i2+1)
    return ""
  
  return string_make_fwork(foreach(0, 0))


