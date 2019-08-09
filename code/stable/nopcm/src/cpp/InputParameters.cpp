#include "InputParameters.hpp"

#include <algorithm>
#include <iostream>
#include <fstream>
#include <iterator>
#include <string>
#include <math.h>
#include <sstream>
#include <limits>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

void get_input(string filename, double &A_C, double &C_W, double &h_C, double &T_init, double &t_final, double &L, double &T_C, double &t_step, double &rho_W, double &D, double &A_tol, double &R_tol, double &T_W, double &E_W) {
    ifstream infile;
    infile.open(filename, std::fstream::in);
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> A_C;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> C_W;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> h_C;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> T_init;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> t_final;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> L;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> T_C;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> t_step;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> rho_W;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> D;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> A_tol;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> R_tol;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> T_W;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile >> E_W;
    infile.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    infile.close();
}

void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) {
    if (!(A_C <= 100000)) {
        std::cout << "Warning: ";
        std::cout << "A_C has value ";
        std::cout << A_C;
        std::cout << " but suggested to be ";
        std::cout << "below ";
        std::cout << 100000;
        std::cout << " (A_C^max)";
        std::cout << "." << std::endl;
    }
    if (!(4170 < C_W && C_W < 4210)) {
        std::cout << "Warning: ";
        std::cout << "C_W has value ";
        std::cout << C_W;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 4170;
        std::cout << " (C_W^min)";
        std::cout << " and ";
        std::cout << 4210;
        std::cout << " (C_W^max)";
        std::cout << "." << std::endl;
    }
    if (!(10 <= h_C && h_C <= 10000)) {
        std::cout << "Warning: ";
        std::cout << "h_C has value ";
        std::cout << h_C;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 10;
        std::cout << " (h_C^min)";
        std::cout << " and ";
        std::cout << 10000;
        std::cout << " (h_C^max)";
        std::cout << "." << std::endl;
    }
    if (!(t_final < 86400)) {
        std::cout << "Warning: ";
        std::cout << "t_final has value ";
        std::cout << t_final;
        std::cout << " but suggested to be ";
        std::cout << "below ";
        std::cout << 86400;
        std::cout << " (t_final^max)";
        std::cout << "." << std::endl;
    }
    if (!(0.1 <= L && L <= 50)) {
        std::cout << "Warning: ";
        std::cout << "L has value ";
        std::cout << L;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 0.1;
        std::cout << " (L_min)";
        std::cout << " and ";
        std::cout << 50;
        std::cout << " (L_max)";
        std::cout << "." << std::endl;
    }
    if (!(950 < rho_W && rho_W <= 1000)) {
        std::cout << "Warning: ";
        std::cout << "rho_W has value ";
        std::cout << rho_W;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 950;
        std::cout << " (rho_W^min)";
        std::cout << " and ";
        std::cout << 1000;
        std::cout << " (rho_W^max)";
        std::cout << "." << std::endl;
    }
    if (!(1.0e-2 <= D && D <= 100)) {
        std::cout << "Warning: ";
        std::cout << "D has value ";
        std::cout << D;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 1.0e-2;
        std::cout << " (AR_min)";
        std::cout << " and ";
        std::cout << 100;
        std::cout << " (AR_max)";
        std::cout << "." << std::endl;
    }
    
    if (!(A_C > 0)) {
        std::cout << "Warning: ";
        std::cout << "A_C has value ";
        std::cout << A_C;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(C_W > 0)) {
        std::cout << "Warning: ";
        std::cout << "C_W has value ";
        std::cout << C_W;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(h_C > 0)) {
        std::cout << "Warning: ";
        std::cout << "h_C has value ";
        std::cout << h_C;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(0 < T_init && T_init < 100)) {
        std::cout << "Warning: ";
        std::cout << "T_init has value ";
        std::cout << T_init;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << 100;
        std::cout << "." << std::endl;
    }
    if (!(t_final > 0)) {
        std::cout << "Warning: ";
        std::cout << "t_final has value ";
        std::cout << t_final;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(L > 0)) {
        std::cout << "Warning: ";
        std::cout << "L has value ";
        std::cout << L;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(0 < T_C && T_C < 100)) {
        std::cout << "Warning: ";
        std::cout << "T_C has value ";
        std::cout << T_C;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << 100;
        std::cout << "." << std::endl;
    }
    if (!(0 < t_step && t_step < t_final)) {
        std::cout << "Warning: ";
        std::cout << "t_step has value ";
        std::cout << t_step;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << 0;
        std::cout << " and ";
        std::cout << t_final;
        std::cout << " (t_final)";
        std::cout << "." << std::endl;
    }
    if (!(rho_W > 0)) {
        std::cout << "Warning: ";
        std::cout << "rho_W has value ";
        std::cout << rho_W;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(D > 0)) {
        std::cout << "Warning: ";
        std::cout << "D has value ";
        std::cout << D;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
    if (!(T_init <= T_W && T_W <= T_C)) {
        std::cout << "Warning: ";
        std::cout << "T_W has value ";
        std::cout << T_W;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << T_init;
        std::cout << " (T_init)";
        std::cout << " and ";
        std::cout << T_C;
        std::cout << " (T_C)";
        std::cout << "." << std::endl;
    }
    if (!(E_W >= 0)) {
        std::cout << "Warning: ";
        std::cout << "E_W has value ";
        std::cout << E_W;
        std::cout << " but suggested to be ";
        std::cout << "above ";
        std::cout << 0;
        std::cout << "." << std::endl;
    }
}

