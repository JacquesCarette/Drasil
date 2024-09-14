module NameGenTest

function helper(temp::Array{Int64})
    result = Int64[]
    
    result = temp[2:3]
    
    @assert length(result) == 2 "Result list should have 2 elements after slicing."
end

global temp = [1, 2, 3]
global result = Int64[]

global result = temp[2:3]

@assert length(result) == 2 "Result list should have 2 elements after slicing."

@assert result[1] == 2 "First element of result should be 2."

end
