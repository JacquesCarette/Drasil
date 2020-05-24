#include "Populate.hpp"

#include <vector>

using std::vector;

Populate::Populate(vector<double> &T_W) : T_W(T_W) {
}

void Populate::operator()(vector<double> &y, double t) {
    T_W.push_back(y.at(0));
}
