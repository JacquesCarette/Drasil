/** main.swift
    Tests various GOOL functions. It should run without errors.
    - Authors: Brooks MacLachlan
*/
import Foundation

extension String: Error {}

// Initializing variables
var a: Int
var b: Int = 5
var myOtherList: [Double] = [1.0, 1.5]
var oneIndex: Int = myOtherList.firstIndex(of: 1.0)!
print(oneIndex)
a = myOtherList.count
myOtherList.insert(2.0, at: 2)
myOtherList.append(2.5)
var e: Double
e = myOtherList[1]
myOtherList[1] = 17.4
var myName: [String] = []
myName = "Brooks Mac".components(separatedBy: " ")
print(myName)
var boringList: [Bool] = [false, false, false, false, false]
print(boringList)
var mySlicedList: [Double] = []

mySlicedList = [Int](stride(from: 1, to: 3, by: 1)).map({(i: Int) -> Double in myOtherList[i]})

if b >= 6 {
    var dummy: String = "dummy"
}
else if b == 5 {
    // If body ----------------------------------------------------------------
    var c: Int
    var d: Int
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
    var myList: [Int] = []
    var myObj: Character = "o"
    let myConst: String = "Imconstant"
    print(a)
    print(b)
    print(c)
    print(d)
    print(myOtherList)
    print(mySlicedList)
    print("Type an int")
    d = Int(readLine()!)!
    print("Type another")
    readLine()!
    
    print(" too")
    print("boo", terminator: "")
    print(true, terminator: "")
    print(0, terminator: "")
    print("c", terminator: "")
    print(!(true))
    print(-1)
    print(sqrt(4.0))
    print(abs(-4))
    print(log10(2.0))
    print(log(2.0))
    print(exp(-2.0))
    print(sin(2.0))
    print(cos(2.0))
    print(tan(2.0))
    print(tan(2.0))
    print(true && false)
    print(true || false)
    print(true && !(false))
    print(!(true && true))
    print(6 + 2)
    print(6 - 2)
    print(6 * 2)
    print(6 / 2)
    print(6 % 4)
    print(Int(pow(Double(6), Double(2))))
    print(6 + 2 * 3)
    print(1.0 / sin(1.0))
    print(1.0 / cos(1.0))
    print(a)
    print(true ? 5 : 0)
    print(1.0 / tan(1.0))
    // End If body ------------------------------------------------------------
}
else {
    print(CommandLine.arguments[5])
}
if boringList != nil {
    print("Ew, boring list!")
}
else {
    print("Great, no bores!")
}
switch a {
    case 5:
        b = 10
    case 0:
        b = 5
    default:
        b = 0
};
for i in [Int](stride(from: 0, to: 9, by: 1)) {
    print(i)
}
while a < 13 {
    print("Hello")
    a += 1;
}
for num in myOtherList {
    print(doubleAndAdd(num, 1.0))
}
do {
    throw "Good-bye!"
} catch {
    print("Caught intentional error")
}
