/** \file Calculations.hpp
    \author Thulasi Jegatheesan
    \brief Provides functions for calculating the outputs
*/
#ifndef Calculations_h
#define Calculations_h

#include <vector>

#include "ODE.hpp"
#include "Populate.hpp"

using std::vector;

/** \brief Calculates volume of water: the amount of space occupied by a given quantity of water (m^3)
    \param V_tank volume of the cylindrical tank: the amount of space encompassed by a tank (m^3)
    \return volume of water: the amount of space occupied by a given quantity of water (m^3)
*/
double func_V_W(double V_tank);

/** \brief Calculates mass of water: the quantity of matter within the water (kg)
    \param rho_W density of water: nass per unit volume of water (kg/m^3)
    \param V_W volume of water: the amount of space occupied by a given quantity of water (m^3)
    \return mass of water: the quantity of matter within the water (kg)
*/
double func_m_W(double rho_W, double V_W);

/** \brief Calculates ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
    \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
    \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
    \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
    \param m_W mass of water: the quantity of matter within the water (kg)
    \return ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
*/
double func_tau_W(double C_W, double h_C, double A_C, double m_W);

/** \brief Calculates temperature of the water: the average kinetic energy of the particles within the water (degreeC)
    \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
    \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
    \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
    \param A_tol absolute tolerance
    \param R_tol relative tolerance
    \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
    \param tau_W ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
    \return temperature of the water: the average kinetic energy of the particles within the water (degreeC)
*/
vector<double> func_T_W(double T_C, double t_final, double T_init, double A_tol, double R_tol, double t_step, double tau_W);

#endif
