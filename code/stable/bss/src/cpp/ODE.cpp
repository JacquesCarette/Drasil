#include "ODE.hpp"

#include <math.h>
#include <vector>

using std::vector;

ODE::ODE(double m_1, double m_2, double x_1_0, double y_1_0, double x_2_0, double y_2_0, double v_x1_0, double v_y1_0, double v_x2_0, double v_y2_0, double t_final) : m_1(m_1), m_2(m_2), x_1_0(x_1_0), y_1_0(y_1_0), x_2_0(x_2_0), y_2_0(y_2_0), v_x1_0(v_x1_0), v_y1_0(v_y1_0), v_x2_0(v_x2_0), v_y2_0(v_y2_0), t_final(t_final) {
}

void ODE::operator()(vector<double> q, vector<double> &dq, double t) {
    dq.at(0) = q.at(4);
    dq.at(1) = q.at(5);
    dq.at(2) = q.at(6);
    dq.at(3) = q.at(7);
    dq.at(4) = -6.6743e-11 * m_2 * (q.at(0) - q.at(2)) / pow(sqrt(pow(q.at(0) - q.at(2), 2.0) + pow(q.at(1) - q.at(3), 2.0)), 3.0);
    dq.at(5) = -6.6743e-11 * m_2 * (q.at(1) - q.at(3)) / pow(sqrt(pow(q.at(0) - q.at(2), 2.0) + pow(q.at(1) - q.at(3), 2.0)), 3.0);
    dq.at(6) = 6.6743e-11 * m_1 * (q.at(0) - q.at(2)) / pow(sqrt(pow(q.at(0) - q.at(2), 2.0) + pow(q.at(1) - q.at(3), 2.0)), 3.0);
    dq.at(7) = 6.6743e-11 * m_1 * (q.at(1) - q.at(3)) / pow(sqrt(pow(q.at(0) - q.at(2), 2.0) + pow(q.at(1) - q.at(3), 2.0)), 3.0);
}
