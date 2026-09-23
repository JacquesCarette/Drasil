function NameGenTest(varargin)
    temp = [1, 2, 3];
    result = [];
    
    temp0 = [];
    i = 1;
    while i < 3
        temp0(length(temp0) + 1) = temp(i + 1);
        i = i + 1;
    end
    result = temp0;
    
    assert(length(result) == 2, "Result list should have 2 elements after slicing.");
    
    assert(result(1) == 2, "First element of result should be 2.");
end

function helper(temp)
    result = [];
    
    temp0 = [];
    i = 1;
    while i < 3
        temp0(length(temp0) + 1) = temp(i + 1);
        i = i + 1;
    end
    result = temp0;
    
    assert(length(result) == 2, "Result list should have 2 elements after slicing.");
end
