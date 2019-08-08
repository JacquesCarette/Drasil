/** \file InputParameters.cs
    \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param A_C heating coil surface area (m^2)
        \param C_W specific heat capacity of water (J/(kg degreeC))
        \param h_C convective heat transfer coefficient between coil and water (W/(m^2 degreeC))
        \param T_init initial temperature (degreeC)
        \param t_final final time (s)
        \param L length of tank (m)
        \param T_C temperature of the heating coil (degreeC)
        \param t_step time step for simulation (s)
        \param rho_W density of water (kg/m^3)
        \param D diameter of tank (m)
        \param A_tol absolute tolerance
        \param R_tol relative tolerance
        \param T_W temperature of the water (degreeC)
        \param E_W change in heat energy in the water (J)
    */
    public static void get_input(string filename, out double A_C, out double C_W, out double h_C, out double T_init, out double t_final, out double L, out double T_C, out double t_step, out double rho_W, out double D, out double A_tol, out double R_tol, out double T_W, out double E_W) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        A_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        C_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        h_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        T_init = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        t_final = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        L = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        T_C = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        t_step = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        rho_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        D = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        A_tol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        R_tol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        T_W = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        E_W = Double.Parse(infile.ReadLine());
        infile.Close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
        \param A_C heating coil surface area (m^2)
        \param C_W specific heat capacity of water (J/(kg degreeC))
        \param h_C convective heat transfer coefficient between coil and water (W/(m^2 degreeC))
        \param T_init initial temperature (degreeC)
        \param t_final final time (s)
        \param L length of tank (m)
        \param T_C temperature of the heating coil (degreeC)
        \param t_step time step for simulation (s)
        \param rho_W density of water (kg/m^3)
        \param D diameter of tank (m)
        \param T_W temperature of the water (degreeC)
        \param E_W change in heat energy in the water (J)
    */
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) {
        if (!(A_C <= 100000)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(4170 < C_W && C_W < 4210)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(10 <= h_C && h_C <= 10000)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(t_final < 86400)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(0.1 <= L && L <= 50)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(950 < rho_W && rho_W <= 1000)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(1.0e-2 <= D && D <= 100)) {
            Console.WriteLine("Warning: constraint violated");
        }
        
        if (!(A_C > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(C_W > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(h_C > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(0 < T_init && T_init < 100)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(t_final > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(L > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(0 < T_C && T_C < 100)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(0 < t_step && t_step < t_final)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(rho_W > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(D > 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(T_init <= T_W && T_W <= T_C)) {
            Console.WriteLine("Warning: constraint violated");
        }
        if (!(E_W >= 0)) {
            Console.WriteLine("Warning: constraint violated");
        }
    }
}

