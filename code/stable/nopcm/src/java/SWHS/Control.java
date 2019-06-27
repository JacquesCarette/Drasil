package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Control {
    
    public static void main(String[] args) throws Exception {
        String filename = args[0];
        double inParams.A_C;
        double inParams.C_W;
        double inParams.h_C;
        double inParams.T_init;
        double inParams.t_final;
        double inParams.L;
        double inParams.T_C;
        double inParams.t_step;
        double inParams.rho_W;
        double inParams.D;
        double inParams.A_tol;
        double inParams.R_tol;
        double inParams.T_W;
        double inParams.E_W;
        Object[] outputs = InputFormat.get_input(filename);
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

