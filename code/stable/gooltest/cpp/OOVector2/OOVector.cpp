#include "OOVector.hpp"

#include <cassert>
#include <iostream>
#include <math.h>
#include <string>
#include <array>

using std::string;

template <std::size_t N>
Vector<N>::Vector(std::array<double, N> v) {
    assert(N > 0 && "Vector dimension must be > 0.");
    this->v = std::array<double, N>();
    std::copy(std::begin(v), std::end(v), std::begin(this->v));
}

template <std::size_t N>
int Vector<N>::dimension() {
    return N;
}

template <std::size_t N>
double Vector<N>::magnitude() {
    return sqrt(Vector::dot(*this, *this));
}

template <std::size_t N>
Vector<N> Vector<N>::norm() {
    double mag = this->magnitude();
    assert(mag > 0.0 && "Cannot normalize a zero vector.");
    return this->scale(1.0 / mag);
}

template <std::size_t N>
double Vector<N>::dot(Vector v1, Vector v2) {
    assert(v1.dimension() == v2.dimension() && "Vector dimensions must match for dot product.");
    double res = 0.0;
    for (int i = 0; i < v1.dimension(); i += 1) {
        res = res + v1.v[i] * v2.v[i];
    }
    return res;
}

template <std::size_t N>
Vector<N> Vector<N>::add(Vector v1, Vector v2) {
  assert(v1.dimension() == v2.dimension() &&
         "Vector dimensions must match for addition.");
    std::array<double, N> res;
    std::copy(std::begin(v1.v), std::end(v1.v), std::begin(res));
    for (int i = 0; i < v1.dimension(); i += 1) {
        res[i] = res[i] + v2.v[i];
    }
    return Vector<N>(res);
}

template <std::size_t N>
Vector<N> Vector<N>::scale(double s) {
    std::array<double, N> res;
    std::copy(std::begin(this->v), std::end(this->v), std::begin(res));
    for (int i = 0; i < this->dimension(); i += 1) {
        res[i] = s * res[i];
    }
    return Vector<N>(res);
}

template <std::size_t N>
void Vector<N>::print() {
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
    std::array<double, 3> ds1 {1.0, 2.0, 3.0};
    std::array<double, 3> ds2 {4.0, 5.0, 6.0};
    Vector<3> v1 = Vector<3>(ds1);
    Vector<3> v2 = Vector<3>(ds2);
    std::cout << "v1: ";
    v1.print();
    std::cout << "v2: ";
    v2.print();
    double d = Vector<3>::dot(v1, v2);
    std::cout << "Dot product: ";
    std::cout << d << std::endl;
    double m = v1.magnitude();
    std::cout << "Magnitude of v1: ";
    std::cout << m << std::endl;
    Vector<3> vAdd = Vector<3>::add(v1, v2); // Note: this has to be done in-line, for some reason
    std::cout << "v1 + v2: ";
    vAdd.print();
    Vector<3> vUnit = Vector<3>::add(v1, v2.scale(2.0)).norm();
    std::cout << "Unit vector of v1 + 2 * v2: ";
    vUnit.print();
    
    return 0;
}
