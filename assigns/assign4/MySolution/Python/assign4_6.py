import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def generator_of_stream(fxs):
  while True:
    cxs = fxs()
    if cxs.ctag == 0:
      break
    else:
      fxs = cxs.cons2
      yield cxs.cons1
  raise StopIteration

def theNatPairs_cubesum():
  i, j = 0, 0
  while True:
    yield (i, j)
    if i == j:
      i = 0
      j += 1
    elif (i ** 3) + (j ** 3) < ((i + 1) ** 3) + (j ** 3):
      i += 1
    else:
      j += 1
    





