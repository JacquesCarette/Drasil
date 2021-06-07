/** \file Control.cs
    \author Thulasi Jegatheesan
    \brief Controls the flow of the program
*/
using System.Collections.Generic;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        double A_C;
        double C_W;
        double h_C;
        double T_init;
        double t_final;
        double L;
        double T_C;
        double t_step;
        double rho_W;
        double D;
        double A_tol;
        double R_tol;
        double E_W;
        double V_tank;
        InputParameters.get_input(filename, out A_C, out C_W, out h_C, out T_init, out t_final, out L, out T_C, out t_step, out rho_W, out D, out A_tol, out R_tol, out E_W);
        V_tank = InputParameters.derived_values(D, L);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, E_W);
        double V_W = Calculations.func_V_W(V_tank);
        double m_W = Calculations.func_m_W(rho_W, V_W);
        double tau_W = Calculations.func_tau_W(C_W, h_C, A_C, m_W);
        List<double> T_W = Calculations.func_T_W(T_C, t_final, T_init, A_tol, R_tol, t_step, tau_W);
        OutputFormat.write_output(E_W, T_W);
    }
}
