package SWHS;

/** \file Calculations.java
    \author Thulasi Jegatheesan
    \brief Provides functions for calculating the outputs
*/
import java.util.ArrayList;
import org.apache.commons.math3.ode.FirstOrderIntegrator;
import org.apache.commons.math3.ode.nonstiff.DormandPrince54Integrator;

public class Calculations {
    
    /** \brief Calculates volume of water: the amount of space occupied by a given quantity of water (m^3)
        \param V_tank volume of the cylindrical tank: the amount of space encompassed by a tank (m^3)
        \return volume of water: the amount of space occupied by a given quantity of water (m^3)
    */
    public static double func_V_W(double V_tank) {
        return V_tank;
    }
    
    /** \brief Calculates mass of water: the quantity of matter within the water (kg)
        \param rho_W density of water: nass per unit volume of water (kg/m^3)
        \param V_W volume of water: the amount of space occupied by a given quantity of water (m^3)
        \return mass of water: the quantity of matter within the water (kg)
    */
    public static double func_m_W(double rho_W, double V_W) {
        return V_W * rho_W;
    }
    
    /** \brief Calculates ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
        \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
        \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
        \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
        \param m_W mass of water: the quantity of matter within the water (kg)
        \return ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
    */
    public static double func_tau_W(double C_W, double h_C, double A_C, double m_W) {
        return m_W * C_W / (h_C * A_C);
    }
    
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
    public static ArrayList<Double> func_T_W(double T_C, double t_final, double T_init, double A_tol, double R_tol, double t_step, double tau_W) {
        ArrayList<Double> T_W;
        ODEStepHandler stepHandler = new ODEStepHandler();
        ODE ode = new ODE(tau_W, T_C);
        double[] curr_vals = {T_init};
        
        FirstOrderIntegrator it = new DormandPrince54Integrator(t_step, t_step, A_tol, R_tol);
        it.addStepHandler(stepHandler);
        it.integrate(ode, 0.0, curr_vals, t_final, curr_vals);
        T_W = stepHandler.T_W;
        
        return T_W;
    }
}
