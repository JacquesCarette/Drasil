#include "OOVector.hpp"

#include <cassert>
#include <iostream>
#include <math.h>
#include <string>
#include <vector>

#include "java.util.Arrays.hpp"

using std::string;
using std::vector;

Vector::Vector(vector<double> v) {
    assert(v.length > 0 && "Vector dimension must be > 0.");
    this->v = v.clone();
}

int Vector::dimension() {
    return this->v.length;
}

double Vector::magnitude() {
    return sqrt(Vector.dot(this, this));
}

Vector Vector::norm() {
    double mag = this.magnitude();
    assert(mag > 0.0 && "Cannot normalize a zero vector.");
    return this.scale(1.0 / mag);
}

double Vector::dot(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for dot product.");
    double res = 0.0;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res = res + v1.v.at(i) * v2.v.at(i);
    }
    return res;
}

Vector Vector::add(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for addition.");
    vector<double> res = v1.v.clone();
    for (int i = 0; i < v1.dimension(); i += 1) {
        res[i] = res.at(i) + v2.v.at(i);
    }
    return Vector(res);
}

Vector Vector::scale(double s) {
    vector<double> res = this->v.clone();
    for (int i = 0; i < this.dimension(); i += 1) {
        res[i] = s * res.at(i);
    }
    return Vector(res);
}

void Vector::print() {
    std::cout << Arrays.toString(this->v) << std::endl;
}

int main(int argc, const char *argv[]) {
    vector<double> ds1 {1.0, 2.0, 3.0};
    vector<double> ds2 {4.0, 5.0, 6.0};
    Vector v1 = Vector(ds1);
    Vector v2 = Vector(ds2);
    std::cout << "v1: ";
    v1.print();
    std::cout << "v2: ";
    v2.print();
    double d = Vector.dot(v1, v2);
    std::cout << "Dot product: ";
    std::cout << d << std::endl;
    double m = v1.magnitude();
    std::cout << "Magnitude of v1: ";
    std::cout << m << std::endl;
    Vector vAdd;
    vAdd = Vector.add(v1, v2);
    std::cout << "v1 + v2: ";
    vAdd.print();
    Vector vUnit;
    vUnit = Vector.add(v1, v2.scale(2.0)).norm();
    std::cout << "Unit vector of v1 + 2 * v2: ";
    vUnit.print();
    
    return 0;
}
