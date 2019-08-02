/** \file InputParameters.hpp
    \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>
#include <vector>

using std::string;
using std::vector;
using std::ifstream;
using std::ofstream;

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param A_C heating coil surface area
    \param C_W specific heat capacity of water
    \param h_C convective heat transfer coefficient between coil and water
    \param T_init initial temperature
    \param t_final final time
    \param L length of tank
    \param T_C temperature of the heating coil
    \param t_step time step for simulation
    \param rho_W density of water
    \param D diameter of tank
    \param A_tol absolute tolerance
    \param R_tol relative tolerance
    \param T_W temperature of the water
    \param E_W change in heat energy in the water
*/
void get_input(string filename, double &A_C, double &C_W, double &h_C, double &T_init, double &t_final, double &L, double &T_C, double &t_step, double &rho_W, double &D, double &A_tol, double &R_tol, double &T_W, double &E_W);

/** \brief Verifies that input values satisfy the physical constraints and software constraints
    \param A_C heating coil surface area
    \param C_W specific heat capacity of water
    \param h_C convective heat transfer coefficient between coil and water
    \param T_init initial temperature
    \param t_final final time
    \param L length of tank
    \param T_C temperature of the heating coil
    \param t_step time step for simulation
    \param rho_W density of water
    \param D diameter of tank
    \param T_W temperature of the water
    \param E_W change in heat energy in the water
*/
void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W);

#endif
