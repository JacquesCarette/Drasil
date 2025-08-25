#include "Populate.hpp"

#include <vector>

using std::vector;

Populate::Populate(vector<double> &y_t) : y_t(y_t) {
}

void Populate::operator()(vector<double> &y, double t) {
    y_t.push_back(y.at(0));
}
