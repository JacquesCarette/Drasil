/** \file Control.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        InputParameters inParams = new InputParameters();
        double g_vect = 9.8;
        double pi = 3.14159265;
        double epsilon = 2.0e-2;
        InputFormat.get_input(inParams, filename);
        InputConstraints.input_constraints(inParams, pi);
        double t_flight = Calculations.func_t_flight(inParams, g_vect);
        double p_land = Calculations.func_p_land(inParams, g_vect);
        double d_offset = Calculations.func_d_offset(inParams, p_land);
        string s = Calculations.func_s(inParams, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}
