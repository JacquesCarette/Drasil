package SWHS;

/** \file Control.java
    \brief Controls the flow of the program
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws Exception {
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
        double T_W;
        double E_W;
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
        T_W = (double)(outputs[12]);
        E_W = (double)(outputs[13]);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
        OutputFormat.write_output(T_W, E_W);
    }
}

