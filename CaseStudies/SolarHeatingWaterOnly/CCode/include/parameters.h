/* Input Parameters Module

This module defines the structure that will hold all of the input
parameters.

Authors: Thulasi Jegatheesan, Spencer Smith, Ned Nedialkov, and Brooks
MacLachlan

Date Last Revised: June 10, 2016

Considerations: The meaning of each parameter is given as a comment in
the code below

*/

#ifndef PARAMETERS_H_INCLUDE
#define PARAMETERS_H_INCLUDE

struct parameters{
    double L;           // Length of tank
    double diam;        // Diameter of tank
    double Vp;          // Volume of PCM
    double Ap;          // Surface area of PCM
    double rho_p;       // Density of PCM
    double Tmelt;       // Melting temperature of PCM
    double C_ps;        // Specific heat capacity of solid PCM
    double C_pl;        // Specific heat capacity of liquid PCM
    double Hf;          // Heat of fusion for PCM
    double Ac;          // Area of coil
    double Tc;          // Temperature of coil
    double rho_w;       // Density of water
    double C_w;         // Specific heat capacity of water
    double hc;          // Heat transfer coefficient between water and coil
    double hp;          // Heat transfer coefficient between PCM and water
    double Tinit;       // Initial temperature of water and PCM
    double tstep;       // Time step for simulation
    double tfinal;      // Time at which to stop simulation
    double AbsTol;      // Absolute tolerance
    double RelTol;      // Relative tolerance
    double ConsTol;     // Relative tolerance for conservation of energy
    double Vt;          // Total volume of tank, including PCM and water
    double Mw;          // Mass of water
    double tau_w;       // ODE parameter for water
    double eta;         // ODE parameter
    double Mp;          // Mass of PCM
    double tau_ps;      // ODE parameter for solid PCM
    double tau_pl;      // ODE parameter for liquid PCM
    double Epmelt_init; // Heat energy in PCM at the instant when melting begins
    double Ep_melt3;    // Heat energy change in PCM from melting start to completion
    double Mw_noPCM;    // Mass of water if no PCM is present
    double tau_w_noPCM; // ODE parameter for water if no PCM is present
};

#endif // PARAMETERS_H_INCLUDE




