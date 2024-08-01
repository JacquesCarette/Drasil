module Helper

""" This function adds two numbers
    - Parameter num1: First number to add
    - Parameter num2: Second number to add
    - Returns: Sum
"""
function doubleAndAdd(num1::Float64, num2::Float64)
    doubledSum = 2.0 * num1 + 2.0 * num2
    return doubledSum
end

end
