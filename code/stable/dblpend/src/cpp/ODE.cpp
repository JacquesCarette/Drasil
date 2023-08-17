#include "ODE.hpp"

#include <math.h>
#include <vector>

using std::vector;

ODE::ODE(double m_1, double m_2, double L_1, double L_2) : m_1(m_1), m_2(m_2), L_1(L_1), L_2(L_2) {
}

void ODE::operator()(vector<double> theta, vector<double> &dtheta, double t) {
    dtheta.at(0) = theta.at(1);
    dtheta.at(1) = (-9.8 * (2.0 * m_1 + m_2) * sin(theta.at(0)) - m_2 * 9.8 * sin(theta.at(0) - 2.0 * theta.at(2)) - 2.0 * sin(theta.at(0) - theta.at(2)) * m_2 * (pow(theta.at(3), 2.0) * L_2 + pow(theta.at(1), 2.0) * L_1 * cos(theta.at(0) - theta.at(2)))) / (L_1 * (2.0 * m_1 + m_2 - m_2 * cos(2.0 * theta.at(0) - 2.0 * theta.at(2))));
    dtheta.at(2) = theta.at(3);
    dtheta.at(3) = 2.0 * sin(theta.at(0) - theta.at(2)) * (pow(theta.at(1), 2.0) * L_1 * (m_1 + m_2) + 9.8 * (m_1 + m_2) * cos(theta.at(0)) + pow(theta.at(3), 2.0) * L_2 * m_2 * cos(theta.at(0) - theta.at(2))) / (L_2 * (2.0 * m_1 + m_2 - m_2 * cos(2.0 * theta.at(0) - 2.0 * theta.at(2))));
}
