import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *


class mylist_nil:
    def __init__(self):
        self.tag = "nil"

    def print(self):
        print(self.value)


class mylist_cons:
    def __init__(self, cons1, cons2):
        self.tag = "cons"
        self.cons1 = cons1
        self.cons2 = cons2

        # return None
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2
    def print(self):
        print(self.cons1)

        print(self.cons2)

class mylist_snoc():
    def __init__(self, cons1, cons2):
        self.tag = "snoc"
        self.cons1 = cons1
        self.cons2 = cons2

        # return None
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2
    def print(self):
        print(self.cons1)
        print(self.cons2)
      
class mylist_reverse():
    def __init__(self, cons1):
        self.tag = "reverse"
        self.cons1 = cons1

class mylist_append2():
    def __init__(self, cons1, cons2):
        #head = cons1
        #while head.next != None:
        #    head = head.next
        #head.next = cons2
        self.tag = "append2"
        self.cons1 = cons1
        self.cons2 = cons2
    def print(self):
        print(self.cons1)
        print(self.cons2)

def mylist_foreach (xs, work):
    if xs.tag == "nil":
        return None
    elif xs.tag == "cons":
        mylist_foreach (xs.cons2, work)
        return work(xs.cons1)
    elif xs.tag == "snoc":
        mylist_foreach (xs.cons1, work)
        return work(xs.cons2)
    elif xs.tag == "reverse":
        mylist_rforeach(xs, work)
    else:
        mylist_foreach (xs.cons1, work)
        mylist_foreach (xs.cons2, work)

def mylist_rforeach (xs, work):
    if xs.tag == "nil":
        return
    elif xs.tag == "cons":
        mylist_rforeach (xs.cons2, work)
        return work(xs.cons1)
    elif xs.tag == "snoc":
        mylist_foreach (xs.cons1, work)
        return work(xs.cons2)
    elif xs.tag == "reverse":
        mylist_foreach(xs.cons1, work)
    else:
        mylist_rforeach (xs.cons2, work)
        mylist_rforeach (xs.cons1, work)
        

        

        
