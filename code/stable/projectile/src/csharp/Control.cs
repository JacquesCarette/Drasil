/** \file Control.cs
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
        InputFormat.get_input(filename, inParams);
        InputConstraints.input_constraints(inParams);
        double t_flight = Calculations.func_t_flight(inParams);
        double p_land = Calculations.func_p_land(inParams);
        double d_offset = Calculations.func_d_offset(inParams, p_land);
        string s = Calculations.func_s(inParams, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}

