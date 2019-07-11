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
        StreamWriter outfile;
        InputParameters inParams = new InputParameters();
        InputFormat.get_input(filename, inParams);
        DerivedValues.derived_values(inParams);
        InputConstraints.input_constraints(inParams);
        double J_tol = Calculations.func_J_tol(inParams);
        double q = Calculations.func_q(inParams);
        double q_hat = Calculations.func_q_hat(inParams, q);
        double q_hat_tol = Calculations.func_q_hat_tol(inParams, J_tol);
        double J = Calculations.func_J(inParams, q_hat);
        double NFL = Calculations.func_NFL(inParams, q_hat_tol);
        double B = Calculations.func_B(inParams, J);
        double LR = Calculations.func_LR(inParams, NFL);
        Boolean is_safeLR = Calculations.func_is_safeLR(LR, q);
        double P_b = Calculations.func_P_b(B);
        Boolean is_safePb = Calculations.func_is_safePb(inParams, P_b);
        OutputFormat.write_output(is_safePb, is_safeLR, P_b);
    }
}

