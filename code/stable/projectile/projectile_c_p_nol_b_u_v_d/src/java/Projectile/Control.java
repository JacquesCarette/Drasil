package Projectile;

/** \file Control.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
import java.io.FileNotFoundException;
import java.io.IOException;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws FileNotFoundException, IOException {
        String filename = args[0];
        double g = 9.8;
        double epsilon = 2.0e-2;
        InputParameters inParams = new InputParameters(filename);
        double t_flight = Calculations.func_t_flight(inParams, g);
        double p_land = Calculations.func_p_land(inParams, g);
        double d_offset = Calculations.func_d_offset(inParams, p_land);
        String s = Calculations.func_s(inParams, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset, t_flight);
    }
}
