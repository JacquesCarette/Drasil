package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputParameters {
    public static double A_C = 0.0;
    public static double C_W = 0.0;
    public static double h_C = 0.0;
    public static double T_init = 0.0;
    public static double t_final = 0.0;
    public static double L = 0.0;
    public static double T_C = 0.0;
    public static double t_step = 0.0;
    public static double rho_W = 0.0;
    public static double D = 0.0;
    public static double A_tol = 0.0;
    public static double R_tol = 0.0;
    public static double T_W = 0.0;
    public static double E_W = 0.0;
    
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double A_tol, double R_tol, double T_W, double E_W) throws Exception {
        if (!((inParams.A_C <= 100000))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((4170 < inParams.C_W) && (inParams.C_W < 4210)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((10 <= inParams.h_C) && (inParams.h_C <= 10000)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.t_final < 86400))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0.1 <= inParams.L) && (inParams.L <= 50)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((950 < inParams.rho_W) && (inParams.rho_W <= 1000)))) {
            System.out.println("Warning: constraint violated");
        }
        
        if (!((inParams.A_C > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.C_W > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.h_C > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < inParams.T_init) && (inParams.T_init < 100)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.t_final > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.L > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < inParams.T_C) && (inParams.T_C < 100)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((0 < inParams.t_step) && (inParams.t_step < inParams.t_final)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.rho_W > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.D > 0))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(((inParams.T_init <= inParams.T_W) && (inParams.T_W <= inParams.T_C)))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.E_W >= 0))) {
            System.out.println("Warning: constraint violated");
        }
    }
}

