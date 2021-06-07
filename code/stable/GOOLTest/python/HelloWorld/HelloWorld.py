## \file HelloWorld.py
# \author Brooks MacLachlan
# \brief Tests various GOOL functions. It should run without errors.
import math
import sys

import Helper

# Initializing variables
b = 5
myOtherList = [1.0, 1.5]
oneIndex = myOtherList.index(1.0)
print(oneIndex)
a = len(myOtherList)
myOtherList.insert(2, 2.0)
myOtherList.append(2.5)
e = myOtherList[1]
myOtherList[1] = 17.4
myName = []
myName = "Brooks Mac".split(" ")
print(myName)
boringList = [False, False, False, False, False]
print(boringList)
mySlicedList = []

mySlicedList = myOtherList[1:3:]

if (b >= 6) :
    dummy = "dummy"
elif (b == 5) :
    # If body -----------------------------------------------------------------
    a = 5
    b = a + 2
    c = b + 3
    d = b
    d -= a
    c -= d
    b += 17;
    c += 17;
    a += 1;
    d += 1;
    c -= 1
    b -= 1
    myList = []
    myObj = 'o'
    myConst = "Imconstant"
    print(a)
    print(b)
    print(c)
    print(d)
    print(myOtherList)
    print(mySlicedList)
    print("Type an int")
    d = int(input())
    print("Type another")
    input()
    
    print(" too")
    print("boo", end="")
    print(True, end="")
    print(0, end="")
    print('c', end="")
    print(not(True))
    print(-1)
    print(math.sqrt(4.0))
    print(math.fabs(-4))
    print(math.log10(2.0))
    print(math.log(2.0))
    print(math.exp(-2.0))
    print(math.sin(2.0))
    print(math.cos(2.0))
    print(math.tan(2.0))
    print(math.tan(2.0))
    print(True and False)
    print(True or False)
    print(True and not(False))
    print(not(True and True))
    print(6 + 2)
    print(6 - 2)
    print(6 * 2)
    print(6 // 2)
    print(6 % 4)
    print(6 ** 2)
    print(6 + 2 * 3)
    print(1.0 / math.sin(1.0))
    print(1.0 / math.cos(1.0))
    print(a)
    print(5 if True else 0)
    print(1.0 / math.tan(1.0))
    # End If body -------------------------------------------------------------
else :
    print(sys.argv[6])
if (boringList != None) :
    print("Ew, boring list!")
else :
    print("Great, no bores!")
if (a == 5) :
    b = 10
elif (a == 0) :
    b = 5
else :
    b = 0
for i in range(0, 9, 1):
    print(i)
while a < 13:
    print("Hello")
    a += 1;
for num in myOtherList:
    print(Helper.doubleAndAdd(num, 1.0))
try :
    raise Exception("Good-bye!")
except Exception :
    print("Caught intentional error")
