def helper(temp):
    result = []
    
    result = temp[1:3:]
    
    assert len(result) == 2, "Result list should have 2 elements after slicing."

temp = [1, 2, 3]
result = []

result = temp[1:3:]

assert len(result) == 2, "Result list should have 2 elements after slicing."

assert result[0] == 2, "First element of result should be 2."
