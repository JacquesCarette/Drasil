module NameGenTest

function helper(temp::Array{Int64})
    result = []
    
    result = temp[2:3]
    
    @assert length(result) == 2 "Result list should have 2 elements after slicing."
end

temp = [1, 2, 3]
result = []

result = temp[2:3]

@assert length(result) == 2 "Result list should have 2 elements after slicing."

@assert result[1] == 2 "First element of result should be 2."

end
