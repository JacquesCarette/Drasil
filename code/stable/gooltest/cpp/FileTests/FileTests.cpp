#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

using std::ifstream;
using std::ofstream;
using std::string;
using std::vector;

int main(int argc, const char *argv[]) {
    ofstream fileToWrite;
    fileToWrite.open("testText.txt", std::fstream::out);
    fileToWrite << 0;
    fileToWrite << 0.89 << std::endl;
    fileToWrite << "ello";
    fileToWrite << "bye" << std::endl;
    fileToWrite << "!!" << std::endl;
    fileToWrite.close();
    ifstream fileToRead;
    fileToRead.open("testText.txt", std::fstream::in);
    string fileLine;
    fileToRead >> fileLine;
    fileToRead.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    fileToRead.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    assert(fileLine != "" && "First line should not be empty.");
    vector<string> fileContents(0);
    
    string nextLine;
    while (std::getline(fileToRead, nextLine)) {
        fileContents.push_back(nextLine);
    }
    
    std::cout << "[";
    for (int list_i1 = 0; list_i1 < (int)(fileContents.size()) - 1; list_i1++) {
        std::cout << fileContents.at(list_i1);
        std::cout << ", ";
    }
    if ((int)(fileContents.size()) > 0) {
        std::cout << fileContents.at((int)(fileContents.size()) - 1);
    }
    std::cout << "]" << std::endl;
    assert((int)(fileContents.size()) > 0 && "fileContents should not be empty.");
    fileToRead.close();
    
    return 0;
}
