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
        String inputfile = args[0];
        InputParameters inParams = new InputParameters();
        InputFormat.func_get_input(inputfile, A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, A_tol, R_tol, T_W, E_W);
        InputParameters.input_constraints(A_C, C_W, h_C, T_init, t_final, L, T_C, t_step, rho_W, D, T_W, E_W);
        OutputFormat.write_output(T_W, E_W);
    }
}

