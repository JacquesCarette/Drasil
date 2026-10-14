private func helper(_ temp: [Int]) -> Void {
    var result: [Int] = []
    
    result = [Int](stride(from: 1, to: 3, by: 1)).map({(i: Int) -> Int in temp[i]})
    
    assert( result.count == 2 , "Result list should have 2 elements after slicing.")
}

var temp: [Int] = [1, 2, 3]
var result: [Int] = []

result = [Int](stride(from: 1, to: 3, by: 1)).map({(i: Int) -> Int in temp[i]})

assert( result.count == 2 , "Result list should have 2 elements after slicing.")

assert( result[0] == 2 , "First element of result should be 2.")
