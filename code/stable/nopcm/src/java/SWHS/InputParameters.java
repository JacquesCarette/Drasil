package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputParameters {
    
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) throws Exception {
        if (!((A_C <= 100000))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((4170 < C_W) && (C_W < 4210)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((10 <= h_C) && (h_C <= 10000)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((t_final < 86400))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0.1 <= L) && (L <= 50)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((950 < rho_W) && (rho_W <= 1000)))) {
            System.out.println("Warning: constraint violated");
        }
        
        if (!((A_C > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((C_W > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((h_C > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < T_init) && (T_init < 100)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((t_final > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((L > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < T_C) && (T_C < 100)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < t_step) && (t_step < t_final)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((rho_W > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((D > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((T_init <= T_W) && (T_W <= T_C)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((E_W >= 0))) {
            System.out.println("Warning: constraint violated");
        }
    }
}

