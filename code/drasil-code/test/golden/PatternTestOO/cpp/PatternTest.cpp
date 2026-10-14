#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "Observer.hpp"

using std::string;
using std::vector;

int main(int argc, const char *argv[]) {
    int n;
    
    std::cout << "myStrat" << std::endl;
    n = 3;
    
    Observer obs1 = Observer();
    Observer obs2 = Observer();
    
    vector<Observer> observerList{obs1};
    observerList.insert(observerList.begin() + (int)(observerList.size()), obs2);
    for (int observerIndex = 0; observerIndex < (int)(observerList.size()); observerIndex++) {
        observerList.at(observerIndex).printNum();
    }
    
    obs1.setX(10);
    std::cout << obs1.getX();
    
    return 0;
}
