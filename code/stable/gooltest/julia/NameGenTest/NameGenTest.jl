module NameGenTest

function helper(temp::Array{Int64})
    result = []
    
    result = temp[2:3]
end

global temp = [1, 2, 3]
global result = []

global result = temp[2:3]

end
