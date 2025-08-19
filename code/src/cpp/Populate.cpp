#include "Populate.hpp"

#include <vector>

using std::vector;

Populate::Populate(vector<double> &theta) : theta(theta) {
}

void Populate::operator()(vector<double> &y, double t) {
    theta.push_back(y.at(0));
}
