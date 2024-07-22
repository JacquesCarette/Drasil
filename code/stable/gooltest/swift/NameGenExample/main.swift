private func helper(_ temp: [Int]) -> Void {
    var result: [Int] = []
    
    result = [Int](stride(from: 1, to: 3, by: 1)).map({(i: Int) -> Int in temp[i]})
}

var temp: [Int] = [1, 2, 3]
var result: [Int] = []

result = [Int](stride(from: 1, to: 3, by: 1)).map({(i: Int) -> Int in temp[i]})

// This shadows a generated name:
var temp0: [Int] = [1, 2, 3]
// This shadows a user-given name:
var result: [Int] = []
