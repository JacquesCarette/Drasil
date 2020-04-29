#include "Observer.hpp"

#include <iostream>

Observer::Observer() : x(5) {
}

void Observer::printNum() {
    std::cout << this->x << std::endl;
}

int Observer::getX() {
    return this->x;
}

void Observer::setX(int x) {
    this->x = x;
}
