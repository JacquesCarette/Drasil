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

#include "Constants.hpp"

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

void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W, Constants &consts) {
    if (!(A_C <= consts.A_C_max)) {
        std::cout << "Warning: ";
        std::cout << "A_C has value ";
        std::cout << A_C;
        std::cout << " but suggested to be ";
        std::cout << "below ";
        std::cout << consts.A_C_max;
        std::cout << " (A_C_max)";
        std::cout << "." << std::endl;
    }
    if (!(consts.C_W_min < C_W && C_W < consts.C_W_max)) {
        std::cout << "Warning: ";
        std::cout << "C_W has value ";
        std::cout << C_W;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << consts.C_W_min;
        std::cout << " (C_W_min)";
        std::cout << " and ";
        std::cout << consts.C_W_max;
        std::cout << " (C_W_max)";
        std::cout << "." << std::endl;
    }
    if (!(consts.h_C_min <= h_C && h_C <= consts.h_C_max)) {
        std::cout << "Warning: ";
        std::cout << "h_C has value ";
        std::cout << h_C;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << consts.h_C_min;
        std::cout << " (h_C_min)";
        std::cout << " and ";
        std::cout << consts.h_C_max;
        std::cout << " (h_C_max)";
        std::cout << "." << std::endl;
    }
    if (!(t_final < consts.t_final_max)) {
        std::cout << "Warning: ";
        std::cout << "t_final has value ";
        std::cout << t_final;
        std::cout << " but suggested to be ";
        std::cout << "below ";
        std::cout << consts.t_final_max;
        std::cout << " (t_final_max)";
        std::cout << "." << std::endl;
    }
    if (!(consts.L_min <= L && L <= consts.L_max)) {
        std::cout << "Warning: ";
        std::cout << "L has value ";
        std::cout << L;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << consts.L_min;
        std::cout << " (L_min)";
        std::cout << " and ";
        std::cout << consts.L_max;
        std::cout << " (L_max)";
        std::cout << "." << std::endl;
    }
    if (!(consts.rho_W_min < rho_W && rho_W <= consts.rho_W_max)) {
        std::cout << "Warning: ";
        std::cout << "rho_W has value ";
        std::cout << rho_W;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << consts.rho_W_min;
        std::cout << " (rho_W_min)";
        std::cout << " and ";
        std::cout << consts.rho_W_max;
        std::cout << " (rho_W_max)";
        std::cout << "." << std::endl;
    }
    if (!(consts.AR_min <= D && D <= consts.AR_max)) {
        std::cout << "Warning: ";
        std::cout << "D has value ";
        std::cout << D;
        std::cout << " but suggested to be ";
        std::cout << "between ";
        std::cout << consts.AR_min;
        std::cout << " (AR_min)";
        std::cout << " and ";
        std::cout << consts.AR_max;
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

