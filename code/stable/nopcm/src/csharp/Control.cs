using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Control {
    
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
        double T_W;
        double E_W;
        InputFormat.get_input(filename, ref A_C, ref C_W, ref h_C, ref T_init, ref t_final, ref L, ref T_C, ref t_step, ref rho_W, ref D, ref A_tol, ref R_tol, ref T_W, ref E_W);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
        OutputFormat.write_output(T_W, E_W);
    }
}

