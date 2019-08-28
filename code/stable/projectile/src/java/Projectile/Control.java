package Projectile;

/** \file Control.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws Exception {
        String filename = args[0];
        InputParameters inParams = new InputParameters();
        double g = 9.8;
        double pi = 3.14159265;
        double epsilon = 2.0e-2;
        InputFormat.get_input(filename, inParams);
        InputConstraints.input_constraints(inParams, pi);
        double t_flight = Calculations.func_t_flight(inParams, g);
        double p_land = Calculations.func_p_land(inParams, g);
        double d_offset = Calculations.func_d_offset(inParams, p_land);
        String s = Calculations.func_s(inParams, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}

