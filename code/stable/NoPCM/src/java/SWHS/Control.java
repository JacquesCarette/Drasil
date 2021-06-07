package SWHS;

/** \file Control.java
    \author Thulasi Jegatheesan
    \brief Controls the flow of the program
*/
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws FileNotFoundException, IOException {
        String filename = args[0];
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
        Object[] outputs = InputParameters.get_input(filename);
        A_C = (double)(outputs[0]);
        C_W = (double)(outputs[1]);
        h_C = (double)(outputs[2]);
        T_init = (double)(outputs[3]);
        t_final = (double)(outputs[4]);
        L = (double)(outputs[5]);
        T_C = (double)(outputs[6]);
        t_step = (double)(outputs[7]);
        rho_W = (double)(outputs[8]);
        D = (double)(outputs[9]);
        A_tol = (double)(outputs[10]);
        R_tol = (double)(outputs[11]);
        E_W = (double)(outputs[12]);
        V_tank = InputParameters.derived_values(D, L);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, E_W);
        double V_W = Calculations.func_V_W(V_tank);
        double m_W = Calculations.func_m_W(rho_W, V_W);
        double tau_W = Calculations.func_tau_W(C_W, h_C, A_C, m_W);
        ArrayList<Double> T_W = Calculations.func_T_W(T_C, t_final, T_init, A_tol, R_tol, t_step, tau_W);
        OutputFormat.write_output(E_W, T_W);
    }
}
