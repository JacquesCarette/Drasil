#ifndef ReadTable_h
#define ReadTable_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;



void func_read_table(string filename, vector<double> &z_vector, vector< vector<double> > &x_matrix, vector< vector<double> > &y_matrix);

#endif
