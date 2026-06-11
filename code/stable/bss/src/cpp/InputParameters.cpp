#include "InputParameters.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <string>

#include "Constants.hpp"

using std::ifstream;
using std::string;

void get_input(string filename, double &m_1, double &m_2, double &x_1_0, double &y_1_0, double &x_2_0, double &y_2_0, double &v_x1_0, double &v_y1_0, double &v_x2_0, double &v_y2_0, double &t_final) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> m_1;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> m_2;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> x_1_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> y_1_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> x_2_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> y_2_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_x1_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_y1_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_x2_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> v_y2_0;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> t_final;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void input_constraints(double m_1, double m_2, double x_1_0, double y_1_0, double x_2_0, double y_2_0, double v_x1_0, double v_y1_0, double v_x2_0, double v_y2_0, double t_final) {
    if (!(Constants::m_min <= m_1 && m_1 <= Constants::m_max)) {
        std::cout << "Warning: ";
        std::cout << "m_1 has value ";
        std::cout << m_1;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << Constants::m_min;
        std::cout << " (m_min)";
        std::cout << " and ";
        std::cout << Constants::m_max;
        std::cout << " (m_max)";
        std::cout << "." << std::endl;
    }
    if (!(Constants::m_min <= m_2 && m_2 <= Constants::m_max)) {
        std::cout << "Warning: ";
        std::cout << "m_2 has value ";
        std::cout << m_2;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << Constants::m_min;
        std::cout << " (m_min)";
        std::cout << " and ";
        std::cout << Constants::m_max;
        std::cout << " (m_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::r_max <= x_1_0 && x_1_0 <= Constants::r_max)) {
        std::cout << "Warning: ";
        std::cout << "x_1_0 has value ";
        std::cout << x_1_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::r_max;
        std::cout << " (-r_max)";
        std::cout << " and ";
        std::cout << Constants::r_max;
        std::cout << " (r_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::r_max <= y_1_0 && y_1_0 <= Constants::r_max)) {
        std::cout << "Warning: ";
        std::cout << "y_1_0 has value ";
        std::cout << y_1_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::r_max;
        std::cout << " (-r_max)";
        std::cout << " and ";
        std::cout << Constants::r_max;
        std::cout << " (r_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::r_max <= x_2_0 && x_2_0 <= Constants::r_max)) {
        std::cout << "Warning: ";
        std::cout << "x_2_0 has value ";
        std::cout << x_2_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::r_max;
        std::cout << " (-r_max)";
        std::cout << " and ";
        std::cout << Constants::r_max;
        std::cout << " (r_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::r_max <= y_2_0 && y_2_0 <= Constants::r_max)) {
        std::cout << "Warning: ";
        std::cout << "y_2_0 has value ";
        std::cout << y_2_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::r_max;
        std::cout << " (-r_max)";
        std::cout << " and ";
        std::cout << Constants::r_max;
        std::cout << " (r_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::v_max <= v_x1_0 && v_x1_0 <= Constants::v_max)) {
        std::cout << "Warning: ";
        std::cout << "v_x1_0 has value ";
        std::cout << v_x1_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::v_max;
        std::cout << " (-v_max)";
        std::cout << " and ";
        std::cout << Constants::v_max;
        std::cout << " (v_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::v_max <= v_y1_0 && v_y1_0 <= Constants::v_max)) {
        std::cout << "Warning: ";
        std::cout << "v_y1_0 has value ";
        std::cout << v_y1_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::v_max;
        std::cout << " (-v_max)";
        std::cout << " and ";
        std::cout << Constants::v_max;
        std::cout << " (v_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::v_max <= v_x2_0 && v_x2_0 <= Constants::v_max)) {
        std::cout << "Warning: ";
        std::cout << "v_x2_0 has value ";
        std::cout << v_x2_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::v_max;
        std::cout << " (-v_max)";
        std::cout << " and ";
        std::cout << Constants::v_max;
        std::cout << " (v_max)";
        std::cout << "." << std::endl;
    }
    if (!(-Constants::v_max <= v_y2_0 && v_y2_0 <= Constants::v_max)) {
        std::cout << "Warning: ";
        std::cout << "v_y2_0 has value ";
        std::cout << v_y2_0;
        std::cout << ", but is suggested to be ";
        std::cout << "between ";
        std::cout << -Constants::v_max;
        std::cout << " (-v_max)";
        std::cout << " and ";
        std::cout << Constants::v_max;
        std::cout << " (v_max)";
        std::cout << "." << std::endl;
    }
    if (!(t_final <= Constants::t_max)) {
        std::cout << "Warning: ";
        std::cout << "t_final has value ";
        std::cout << t_final;
        std::cout << ", but is suggested to be ";
        std::cout << "below ";
        std::cout << Constants::t_max;
        std::cout << " (t_max)";
        std::cout << "." << std::endl;
    }
    
    if (!(m_1 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "m_1 has value ";
        std::cout << m_1;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(m_2 > 0.0)) {
        std::cout << "Warning: ";
        std::cout << "m_2 has value ";
        std::cout << m_2;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
    if (!(t_final >= 0.0)) {
        std::cout << "Warning: ";
        std::cout << "t_final has value ";
        std::cout << t_final;
        std::cout << ", but is suggested to be ";
        std::cout << "above ";
        std::cout << 0.0;
        std::cout << "." << std::endl;
    }
}
