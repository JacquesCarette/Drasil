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
        double v_launch;
        double theta;
        double p_target;
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        Object[] outputs = InputFormat.get_input(filename);
        v_launch = (double)(outputs[0]);
        theta = (double)(outputs[1]);
        p_target = (double)(outputs[2]);
        InputConstraints.input_constraints(v_launch, theta, p_target);
        double t_flight = Calculations.func_t_flight(v_launch, theta, g_vect);
        double p_land = Calculations.func_p_land(v_launch, theta, g_vect);
        double d_offset = Calculations.func_d_offset(p_target, p_land);
        String s = Calculations.func_s(p_target, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}
