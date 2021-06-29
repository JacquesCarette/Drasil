/** \file HelloWorld.cpp
    \author Brooks MacLachlan
    \brief Tests various GOOL functions. It should run without errors.
*/
#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>
#include <math.h>
#include <sstream>
#include <string>
#include <vector>

#include "Helper.hpp"

using std::string;
using std::vector;

int main(int argc, const char *argv[]) {
    // Initializing variables
    int a;
    int b = 5;
    vector<double> myOtherList{1.0, 1.5};
    int oneIndex = find(myOtherList.begin(), myOtherList.end(), 1.0) - myOtherList.begin();
    std::cout << oneIndex << std::endl;
    a = (int)(myOtherList.size());
    myOtherList.insert(myOtherList.begin() + 2, 2.0);
    myOtherList.push_back(2.5);
    double e;
    e = myOtherList.at(1);
    myOtherList.at(1) = 17.4;
    vector<string> myName(7);
    myName.clear();
    std::stringstream ss;
    ss.str("Brooks Mac");
    string word;
    while (std::getline(ss, word, ' ')) {
        myName.push_back(word);
    }
    std::cout << "[";
    for (int list_i1 = 0; list_i1 < (int)(myName.size()) - 1; list_i1++) {
        std::cout << myName.at(list_i1);
        std::cout << ", ";
    }
    if ((int)(myName.size()) > 0) {
        std::cout << myName.at((int)(myName.size()) - 1);
    }
    std::cout << "]" << std::endl;
    vector<bool> boringList{false, false, false, false, false};
    std::cout << "[";
    for (int list_i1 = 0; list_i1 < (int)(boringList.size()) - 1; list_i1++) {
        std::cout << boringList.at(list_i1);
        std::cout << ", ";
    }
    if ((int)(boringList.size()) > 0) {
        std::cout << boringList.at((int)(boringList.size()) - 1);
    }
    std::cout << "]" << std::endl;
    vector<double> mySlicedList(2);
    
    vector<double> temp(0);
    for (int i_temp = 1; i_temp < 3; i_temp++) {
        temp.push_back(myOtherList.at(i_temp));
    }
    mySlicedList = temp;
    
    if (b >= 6) {
        string dummy = "dummy";
    }
    else if (b == 5) {
        // If body ----------------------------------------------------------------
        int c;
        int d;
        a = 5;
        b = a + 2;
        c = b + 3;
        d = b;
        d -= a;
        c -= d;
        b += 17;
        c += 17;
        a++;
        d++;
        c--;
        b--;
        vector<int> myList(5);
        char myObj = 'o';
        const string myConst = "Imconstant";
        std::cout << a << std::endl;
        std::cout << b << std::endl;
        std::cout << c << std::endl;
        std::cout << d << std::endl;
        std::cout << "[";
        for (int list_i1 = 0; list_i1 < (int)(myOtherList.size()) - 1; list_i1++) {
            std::cout << myOtherList.at(list_i1);
            std::cout << ", ";
        }
        if ((int)(myOtherList.size()) > 0) {
            std::cout << myOtherList.at((int)(myOtherList.size()) - 1);
        }
        std::cout << "]" << std::endl;
        std::cout << "[";
        for (int list_i1 = 0; list_i1 < (int)(mySlicedList.size()) - 1; list_i1++) {
            std::cout << mySlicedList.at(list_i1);
            std::cout << ", ";
        }
        if ((int)(mySlicedList.size()) > 0) {
            std::cout << mySlicedList.at((int)(mySlicedList.size()) - 1);
        }
        std::cout << "]" << std::endl;
        std::cout << "Type an int" << std::endl;
        std::cin >> d;
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << "Type another" << std::endl;
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        
        std::cout << " too" << std::endl;
        std::cout << "boo";
        std::cout << true;
        std::cout << 0;
        std::cout << 'c';
        std::cout << !(true) << std::endl;
        std::cout << -1 << std::endl;
        std::cout << sqrt(4.0) << std::endl;
        std::cout << fabs(-4) << std::endl;
        std::cout << log10(2.0) << std::endl;
        std::cout << log(2.0) << std::endl;
        std::cout << exp(-2.0) << std::endl;
        std::cout << sin(2.0) << std::endl;
        std::cout << cos(2.0) << std::endl;
        std::cout << tan(2.0) << std::endl;
        std::cout << tan(2.0) << std::endl;
        std::cout << (true && false) << std::endl;
        std::cout << (true || false) << std::endl;
        std::cout << (true && !(false)) << std::endl;
        std::cout << !(true && true) << std::endl;
        std::cout << (6 + 2) << std::endl;
        std::cout << (6 - 2) << std::endl;
        std::cout << (6 * 2) << std::endl;
        std::cout << (6 / 2) << std::endl;
        std::cout << (6 % 4) << std::endl;
        std::cout << pow(6, 2) << std::endl;
        std::cout << (6 + 2 * 3) << std::endl;
        std::cout << (1.0 / sin(1.0)) << std::endl;
        std::cout << (1.0 / cos(1.0)) << std::endl;
        std::cout << a << std::endl;
        std::cout << (true ? 5 : 0) << std::endl;
        std::cout << (1.0 / tan(1.0)) << std::endl;
        // End If body ------------------------------------------------------------
    }
    else {
        std::cout << argv[6] << std::endl;
    }
    std::cout << "Ew, boring list!" << std::endl;
    switch(a) {
        case 5:
            b = 10;
            break;
        case 0:
            b = 5;
            break;
        default:
            b = 0;
            break;
    };
    for (int i = 0; i < 9; i += 1) {
        std::cout << i << std::endl;
    }
    while (a < 13) {
        std::cout << "Hello" << std::endl;
        a++;
    }
    for (std::vector<double>::iterator num = myOtherList.begin(); num != myOtherList.end(); num++) {
        std::cout << doubleAndAdd((*num), 1.0) << std::endl;
    }
    try {
        throw("Good-bye!");
    } catch (...) {
        std::cout << "Caught intentional error" << std::endl;
    }
    
    return 0;
}
