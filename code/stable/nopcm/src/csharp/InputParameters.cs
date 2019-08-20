/** \file InputParameters.cs
    \author Thulasi Jegatheesan
    \brief Provides the function for reading inputs and the function for checking the physical constraints and software constraints on the input
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
        \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
        \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
        \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
        \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
        \param L length of tank: the length of the tank (m)
        \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
        \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
        \param rho_W density of water: nass per unit volume of water (kg/m^3)
        \param D diameter of tank: the diameter of the tank (m)
        \param A_tol absolute tolerance
        \param R_tol relative tolerance
        \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
        \param E_W change in heat energy in the water: change in thermal energy within the water (J)
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
        \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
        \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
        \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
        \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
        \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
        \param L length of tank: the length of the tank (m)
        \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
        \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
        \param rho_W density of water: nass per unit volume of water (kg/m^3)
        \param D diameter of tank: the diameter of the tank (m)
        \param T_W temperature of the water: the average kinetic energy of the particles within the water (degreeC)
        \param E_W change in heat energy in the water: change in thermal energy within the water (J)
    */
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W) {
        if (!(A_C <= 100000)) {
            Console.Write("Warning: ");
            Console.Write("A_C has value ");
            Console.Write(A_C);
            Console.Write(" but suggested to be ");
            Console.Write("below ");
            Console.Write(100000);
            Console.Write(" (A_C^max)");
            Console.WriteLine(".");
        }
        if (!(4170 < C_W && C_W < 4210)) {
            Console.Write("Warning: ");
            Console.Write("C_W has value ");
            Console.Write(C_W);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(4170);
            Console.Write(" (C_W^min)");
            Console.Write(" and ");
            Console.Write(4210);
            Console.Write(" (C_W^max)");
            Console.WriteLine(".");
        }
        if (!(10 <= h_C && h_C <= 10000)) {
            Console.Write("Warning: ");
            Console.Write("h_C has value ");
            Console.Write(h_C);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(10);
            Console.Write(" (h_C^min)");
            Console.Write(" and ");
            Console.Write(10000);
            Console.Write(" (h_C^max)");
            Console.WriteLine(".");
        }
        if (!(t_final < 86400)) {
            Console.Write("Warning: ");
            Console.Write("t_final has value ");
            Console.Write(t_final);
            Console.Write(" but suggested to be ");
            Console.Write("below ");
            Console.Write(86400);
            Console.Write(" (t_final^max)");
            Console.WriteLine(".");
        }
        if (!(0.1 <= L && L <= 50)) {
            Console.Write("Warning: ");
            Console.Write("L has value ");
            Console.Write(L);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0.1);
            Console.Write(" (L_min)");
            Console.Write(" and ");
            Console.Write(50);
            Console.Write(" (L_max)");
            Console.WriteLine(".");
        }
        if (!(950 < rho_W && rho_W <= 1000)) {
            Console.Write("Warning: ");
            Console.Write("rho_W has value ");
            Console.Write(rho_W);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(950);
            Console.Write(" (rho_W^min)");
            Console.Write(" and ");
            Console.Write(1000);
            Console.Write(" (rho_W^max)");
            Console.WriteLine(".");
        }
        if (!(1.0e-2 <= D && D <= 100)) {
            Console.Write("Warning: ");
            Console.Write("D has value ");
            Console.Write(D);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(1.0e-2);
            Console.Write(" (AR_min)");
            Console.Write(" and ");
            Console.Write(100);
            Console.Write(" (AR_max)");
            Console.WriteLine(".");
        }
        
        if (!(A_C > 0)) {
            Console.Write("Warning: ");
            Console.Write("A_C has value ");
            Console.Write(A_C);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(C_W > 0)) {
            Console.Write("Warning: ");
            Console.Write("C_W has value ");
            Console.Write(C_W);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(h_C > 0)) {
            Console.Write("Warning: ");
            Console.Write("h_C has value ");
            Console.Write(h_C);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(0 < T_init && T_init < 100)) {
            Console.Write("Warning: ");
            Console.Write("T_init has value ");
            Console.Write(T_init);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(100);
            Console.WriteLine(".");
        }
        if (!(t_final > 0)) {
            Console.Write("Warning: ");
            Console.Write("t_final has value ");
            Console.Write(t_final);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(L > 0)) {
            Console.Write("Warning: ");
            Console.Write("L has value ");
            Console.Write(L);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(0 < T_C && T_C < 100)) {
            Console.Write("Warning: ");
            Console.Write("T_C has value ");
            Console.Write(T_C);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(100);
            Console.WriteLine(".");
        }
        if (!(0 < t_step && t_step < t_final)) {
            Console.Write("Warning: ");
            Console.Write("t_step has value ");
            Console.Write(t_step);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(t_final);
            Console.Write(" (t_final)");
            Console.WriteLine(".");
        }
        if (!(rho_W > 0)) {
            Console.Write("Warning: ");
            Console.Write("rho_W has value ");
            Console.Write(rho_W);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(D > 0)) {
            Console.Write("Warning: ");
            Console.Write("D has value ");
            Console.Write(D);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(T_init <= T_W && T_W <= T_C)) {
            Console.Write("Warning: ");
            Console.Write("T_W has value ");
            Console.Write(T_W);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(T_init);
            Console.Write(" (T_init)");
            Console.Write(" and ");
            Console.Write(T_C);
            Console.Write(" (T_C)");
            Console.WriteLine(".");
        }
        if (!(E_W >= 0)) {
            Console.Write("Warning: ");
            Console.Write("E_W has value ");
            Console.Write(E_W);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
    }
}

