#include "OOVector.hpp"

#include <cassert>
#include <iostream>
#include <math.h>
#include <string>
#include <vector>

using std::string;
using std::vector;

Vector::Vector(vector<double> v) {
    assert((int)(v.size()) > 0 && "Vector dimension must be > 0.");
    this->v = v;
}

int Vector::dimension() {
    return (int)(this->v.size());
}

double Vector::magnitude() {
    return sqrt(Vector::dot(*this, *this));
}

Vector Vector::norm(Vector v) {
    double mag = v.magnitude();
    assert(mag > 0.0 && "Cannot normalize a zero vector.");
    return Vector::scale(v, 1.0 / mag);
}

double Vector::dot(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for dot product.");
    double res = 0.0;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res += v1.v.at(i) * v2.v.at(i);
    }
    return res;
}

Vector Vector::add(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for addition.");
    vector<double> res = v1.v;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res[i] += v2.v.at(i);
    }
    return Vector(res);
}

Vector Vector::scale(Vector v, double s) {
    vector<double> res = v.v;
    for (int i = 0; i < v.dimension(); i += 1) {
        res[i] = s * res.at(i);
    }
    return Vector(res);
}

void Vector::printSelf() {
    std::cout << "[";
    for (int list_i1 = 0; list_i1 < (int)(this->v.size()) - 1; list_i1++) {
        std::cout << this->v.at(list_i1);
        std::cout << ", ";
    }
    if ((int)(this->v.size()) > 0) {
        std::cout << this->v.at((int)(this->v.size()) - 1);
    }
    std::cout << "]" << std::endl;
}

int main(int argc, const char *argv[]) {
    vector<double> ds1 {1.0, 2.0, 3.0};
    vector<double> ds2 {4.0, 5.0, 6.0};
    Vector v1 = Vector(ds1);
    Vector v2 = Vector(ds2);
    std::cout << "v1: ";
    v1.printSelf();
    std::cout << "v2: ";
    v2.printSelf();
    double d = Vector::dot(v1, v2);
    std::cout << "Dot product: ";
    std::cout << d << std::endl;
    double m = v1.magnitude();
    std::cout << "Magnitude of v1: ";
    std::cout << m << std::endl;
    Vector vAdd = Vector::add(v1, v2);
    std::cout << "v1 + v2: ";
    vAdd.printSelf();
    Vector vUnit = Vector::norm(Vector::add(v1, Vector::scale(v2, 2.0)));
    std::cout << "Unit vector of v1 + 2 * v2: ";
    vUnit.printSelf();
    
    return 0;
}
