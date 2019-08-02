package SWHS;

/** \file InputParameters.java
    \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
    */
    public static Object[] get_input(String filename) throws Exception {
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
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        A_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        C_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        h_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        T_init = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        t_final = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        L = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        T_C = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        t_step = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        rho_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        D = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        A_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        R_tol = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        T_W = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        E_W = Double.parseDouble(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[14];
        outputs[0] = A_C;
        outputs[1] = C_W;
        outputs[2] = h_C;
        outputs[3] = T_init;
        outputs[4] = t_final;
        outputs[5] = L;
        outputs[6] = T_C;
        outputs[7] = t_step;
        outputs[8] = rho_W;
        outputs[9] = D;
        outputs[10] = A_tol;
        outputs[11] = R_tol;
        outputs[12] = T_W;
        outputs[13] = E_W;
        return outputs;
    }
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param A_C heating coil surface area
        \param C_W specific heat capacity of water
        \param h_C convective heat transfer coefficient between coil and water
        \param T_init initial temperature
        \param t_final final time
        \param L length of tank
        \param T_C temperature of the heating coil
        \param t_step time step for simulation
        \param rho_W density of water
        \param D diameter of tank
        \param T_W temperature of the water
        \param E_W change in heat energy in the water
    */
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) throws Exception {
        if (!(A_C <= 100000)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(4170 < C_W && C_W < 4210)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(10 <= h_C && h_C <= 10000)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(t_final < 86400)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0.1 <= L && L <= 50)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(950 < rho_W && rho_W <= 1000)) {
            System.out.println("Warning: constraint violated");
        }
        
        if (!(A_C > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(C_W > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(h_C > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0 < T_init && T_init < 100)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(t_final > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(L > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0 < T_C && T_C < 100)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0 < t_step && t_step < t_final)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(rho_W > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(D > 0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(T_init <= T_W && T_W <= T_C)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(E_W >= 0)) {
            System.out.println("Warning: constraint violated");
        }
    }
}

