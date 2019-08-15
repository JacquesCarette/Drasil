/** \file InputParameters.hpp
    \author Thulasi Jegatheesan
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
    \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
    \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
    \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
    \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
    \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
    \param L length of tank: the length of the tank (m)
    \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
    \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
    \param rho_W density of water: nass per unit volume of water (kg/m^3)
    \param D diameter of tank: the diameter of the tank (m)
    \param A_tol absolute tolerance
    \param R_tol relative tolerance
    \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
    \param E_W change in heat energy in the water: change in thermal energy within the water (J)
*/
void get_input(string filename, double &A_C, double &C_W, double &h_C, double &T_init, double &t_final, double &L, double &T_C, double &t_step, double &rho_W, double &D, double &A_tol, double &R_tol, double &T_W, double &E_W);

/** \brief Verifies that input values satisfy the physical constraints and software constraints
    \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
    \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
    \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
    \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
    \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
    \param L length of tank: the length of the tank (m)
    \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
    \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
    \param rho_W density of water: nass per unit volume of water (kg/m^3)
    \param D diameter of tank: the diameter of the tank (m)
    \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
    \param E_W change in heat energy in the water: change in thermal energy within the water (J)
*/
void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W);

#endif
