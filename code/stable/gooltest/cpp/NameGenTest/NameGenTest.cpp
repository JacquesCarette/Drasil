#include "NameGenTest.hpp"

#include <cassert>
#include <string>
#include <vector>

using std::string;
using std::vector;

int main(int argc, const char *argv[]) {
    vector<int> temp{1, 2, 3};
    vector<int> result(2);
    
    vector<int> temp0(0);
    for (int i = 1; i < 3; i++) {
        temp0.push_back(temp.at(i));
    }
    result = temp0;
    
    assert((int)(result.size()) == 2 && "Result list should have 2 elements after slicing.");
    
    assert(result.at(0) == 2 && "First element of result should be 2.");
    
    return 0;
}

void helper(vector<int> temp) {
    vector<int> result(2);
    
    vector<int> temp0(0);
    for (int i = 1; i < 3; i++) {
        temp0.push_back(temp.at(i));
    }
    result = temp0;
    
    assert((int)(result.size()) == 2 && "Result list should have 2 elements after slicing.");
}
