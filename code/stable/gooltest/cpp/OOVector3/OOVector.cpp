#include "OOVector.hpp"

#include <cassert>
#include <iostream>
#include <math.h>
#include <string>
#include <vector>

using std::string;

Vector::Vector(std::vector<double> v) {
    assert(v.size() > 0 && "Vector dimension must be > 0.");
    this->v = v;
}

int Vector::dimension() {
    return this->v.size();
}

double Vector::magnitude() {
    return sqrt(Vector::dot(*this, *this));
}

Vector Vector::norm() {
    double mag = this->magnitude();
    assert(mag > 0.0 && "Cannot normalize a zero vector.");
    return this->scale(1.0 / mag);
}

double Vector::dot(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for dot product.");
    double res = 0.0;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res = res + v1.v[i] * v2.v[i];
    }
    return res;
}

Vector Vector::add(Vector v1, Vector v2) {
  assert(v1.dimension() == v2.dimension() &&
         "Vector dimensions must match for addition.");
    std::vector<double> res = v1.v;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res[i] = res[i] + v2.v[i];
    }
    return Vector(res);
}

Vector Vector::scale(double s) {
    std::vector<double> res = this->v;
    for (int i = 0; i < this->dimension(); i += 1) {
        res[i] = s * res[i];
    }
    return Vector(res);
}

void Vector::print() {
    std::cout << "[";
    for (int i = 0; i < this->dimension(); i++) {
        if (i > 0) {
            std::cout << ", ";
        }
        std::cout << this->v[i];
    }
    std::cout << "]" << std::endl;
}

int main(int argc, const char *argv[]) {
    std::vector<double> ds1 {1.0, 2.0, 3.0};
    std::vector<double> ds2 {4.0, 5.0, 6.0};
    Vector v1 = Vector(ds1);
    Vector v2 = Vector(ds2);
    std::cout << "v1: ";
    v1.print();
    std::cout << "v2: ";
    v2.print();
    double d = Vector::dot(v1, v2);
    std::cout << "Dot product: ";
    std::cout << d << std::endl;
    double m = v1.magnitude();
    std::cout << "Magnitude of v1: ";
    std::cout << m << std::endl;
    Vector vAdd = Vector::add(v1, v2); // Note: this has to be done in-line, for some reason
    std::cout << "v1 + v2: ";
    vAdd.print();
    Vector vUnit = Vector::add(v1, v2.scale(2.0)).norm();
    std::cout << "Unit vector of v1 + 2 * v2: ";
    vUnit.print();
    
    return 0;
}
