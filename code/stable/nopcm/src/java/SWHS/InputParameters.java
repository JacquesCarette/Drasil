package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class InputParameters {
    public static double A_C;
    public static double C_W;
    public static double h_C;
    public static double T_init;
    public static double t_final;
    public static double L;
    public static double T_C;
    public static double ρ_W;
    public static double D;
    public static double T_W;
    public static double E_W;
    
    public static void derived_values() throws Exception {
    }
    
    public static void input_constraints(InputParameters inParams) throws Exception {
        if (!(inParams.A_C <= A_C_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((C_W_min < inParams.C_W) && (inParams.C_W < C_W_max))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((h_C_min <= inParams.h_C) && (inParams.h_C <= h_C_max))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.t_final < t_final_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((L_min <= inParams.L) && (inParams.L <= L_max))) {
            System.out.println("Warning: constraint violated");
        }
        if (!((ρ_W_min < inParams.ρ_W) && (inParams.ρ_W <= ρ_W_max))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.A_C > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.C_W > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.h_C > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((0 < inParams.T_init) && (inParams.T_init < 100))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.t_final > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.L > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((0 < inParams.T_C) && (inParams.T_C < 100))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.ρ_W > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.D > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((inParams.T_init <= inParams.T_W) && (inParams.T_W <= inParams.T_C))) {
            System.out.println("Warning: constraint violated");
        }
        if (!(inParams.E_W >= 0)) {
            System.out.println("Warning: constraint violated");
        }
    }
}

