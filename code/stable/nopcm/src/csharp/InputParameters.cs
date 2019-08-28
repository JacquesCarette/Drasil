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
        \param consts structure holding the constant values
    */
    public static void input_constraints(double A_C, double C_W, double h_C, double T_init, double t_final, double L, double T_C, double t_step, double rho_W, double D, double T_W, double E_W, Constants consts) {
        if (!(A_C <= consts.A_C_max)) {
            Console.Write("Warning: ");
            Console.Write("A_C has value ");
            Console.Write(A_C);
            Console.Write(" but suggested to be ");
            Console.Write("below ");
            Console.Write(consts.A_C_max);
            Console.Write(" (A_C_max)");
            Console.WriteLine(".");
        }
        if (!(consts.C_W_min < C_W && C_W < consts.C_W_max)) {
            Console.Write("Warning: ");
            Console.Write("C_W has value ");
            Console.Write(C_W);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(consts.C_W_min);
            Console.Write(" (C_W_min)");
            Console.Write(" and ");
            Console.Write(consts.C_W_max);
            Console.Write(" (C_W_max)");
            Console.WriteLine(".");
        }
        if (!(consts.h_C_min <= h_C && h_C <= consts.h_C_max)) {
            Console.Write("Warning: ");
            Console.Write("h_C has value ");
            Console.Write(h_C);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(consts.h_C_min);
            Console.Write(" (h_C_min)");
            Console.Write(" and ");
            Console.Write(consts.h_C_max);
            Console.Write(" (h_C_max)");
            Console.WriteLine(".");
        }
        if (!(t_final < consts.t_final_max)) {
            Console.Write("Warning: ");
            Console.Write("t_final has value ");
            Console.Write(t_final);
            Console.Write(" but suggested to be ");
            Console.Write("below ");
            Console.Write(consts.t_final_max);
            Console.Write(" (t_final_max)");
            Console.WriteLine(".");
        }
        if (!(consts.L_min <= L && L <= consts.L_max)) {
            Console.Write("Warning: ");
            Console.Write("L has value ");
            Console.Write(L);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(consts.L_min);
            Console.Write(" (L_min)");
            Console.Write(" and ");
            Console.Write(consts.L_max);
            Console.Write(" (L_max)");
            Console.WriteLine(".");
        }
        if (!(consts.rho_W_min < rho_W && rho_W <= consts.rho_W_max)) {
            Console.Write("Warning: ");
            Console.Write("rho_W has value ");
            Console.Write(rho_W);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(consts.rho_W_min);
            Console.Write(" (rho_W_min)");
            Console.Write(" and ");
            Console.Write(consts.rho_W_max);
            Console.Write(" (rho_W_max)");
            Console.WriteLine(".");
        }
        if (!(consts.AR_min <= D && D <= consts.AR_max)) {
            Console.Write("Warning: ");
            Console.Write("D has value ");
            Console.Write(D);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(consts.AR_min);
            Console.Write(" (AR_min)");
            Console.Write(" and ");
            Console.Write(consts.AR_max);
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

