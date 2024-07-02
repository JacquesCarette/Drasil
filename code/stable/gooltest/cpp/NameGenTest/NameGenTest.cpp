#include "NameGenTest.hpp"

#include <vector>

using std::vector;

int main(int argc, const char *argv[]) {
    vector<int> temp{1, 2, 3};
    vector<int> result(2);
    
    vector<int> temp0(0);
    for (int i = 1; i < 3; i++) {
        temp0.push_back(temp.at(i));
    }
    result = temp0;
    
    return 0;
}

void helper(vector<int> temp) {
    vector<int> result(2);
    
    vector<int> temp0(0);
    for (int i = 1; i < 3; i++) {
        temp0.push_back(temp.at(i));
    }
    result = temp0;
}
