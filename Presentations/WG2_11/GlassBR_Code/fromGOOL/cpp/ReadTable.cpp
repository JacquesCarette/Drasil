#include "ReadTable.hpp"


#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <vector>

using namespace GlassBR;
using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

vector<double> GlassBR::read_z_array(string filename) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    string line;
    std::getline(infile, line);
    infile.close();
    vector<string> z_array_str(0);
    {   std::stringstream ss;
        ss.str(line);
        string word;
        while (std::getline(ss, word, ',')) {
            z_array_str.push_back(word);
        }
    }
    {   vector<string> temp(0);
        for (int i = 1; i < z_array_str.size(); i += 2) {
            temp.push_back(z_array_str.at(i));
        }
        z_array_str = temp;
    }
    vector<double> z_array(0);
    for (int i = 0; i < z_array_str.size(); i++) {
        z_array.push_back(std::stod(z_array_str.at(i)));
    }
    return z_array;
}

vector< vector<double> > GlassBR::read_x_array(string filename) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    vector<string> lines(0);
    string nextLine;
    while (std::getline(infile, nextLine)) {
        lines.push_back(nextLine);
    }
    infile.close();
    {   vector<string> temp(0);
        for (int i = 1; i < lines.size(); i++) {
            temp.push_back(lines.at(i));
        }
        lines = temp;
    }
    vector< vector<string> > x_array_str(0);
    for (int i = 0; i < lines.size(); i++) {
        vector<string> temp_str(0);
        {   std::stringstream ss;
            ss.str(lines.at(i));
            string word;
            while (std::getline(ss, word, ',')) {
                temp_str.push_back(word);
            }
        }
        {   vector<string> temp(0);
            for (int i = 0; i < temp_str.size(); i += 2) {
                temp.push_back(temp_str.at(i));
            }
            temp_str = temp;
        }
        x_array_str.push_back(temp_str);
    }
    vector< vector<double> > x_array(0);
    for (int i = 0; i < x_array_str.size(); i++) {
        vector<double> nextLine(0);
        for (int j = 0; j < x_array_str.at(i).size(); j++) {
            nextLine.push_back(std::stod(x_array_str.at(i).at(j)));
        }
        x_array.push_back(nextLine);
    }
    return x_array;
}

vector< vector<double> > GlassBR::read_y_array(string filename) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    vector<string> lines(0);
    string nextLine;
    while (std::getline(infile, nextLine)) {
        lines.push_back(nextLine);
    }
    infile.close();
    {   vector<string> temp(0);
        for (int i = 1; i < lines.size(); i++) {
            temp.push_back(lines.at(i));
        }
        lines = temp;
    }
    vector< vector<string> > y_array_str(0);
    for (int i = 0; i < lines.size(); i++) {
        vector<string> temp_str(0);
        {   std::stringstream ss;
            ss.str(lines.at(i));
            string word;
            while (std::getline(ss, word, ',')) {
                temp_str.push_back(word);
            }
        }
        {   vector<string> temp(0);
            for (int i = 1; i < temp_str.size(); i += 2) {
                temp.push_back(temp_str.at(i));
            }
            temp_str = temp;
        }
        y_array_str.push_back(temp_str);
    }
    vector< vector<double> > y_array(0);
    for (int i = 0; i < y_array_str.size(); i++) {
        vector<double> nextLine(0);
        for (int j = 0; j < y_array_str.at(i).size(); j++) {
            nextLine.push_back(std::stod(y_array_str.at(i).at(j)));
        }
        y_array.push_back(nextLine);
    }
    return y_array;
}

