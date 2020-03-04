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
        float v_launch;
        float theta;
        float p_target;
        float g_vect = 9.8f;
        float epsilon = 2.0e-2f;
        Object[] outputs = InputFormat.get_input(filename);
        v_launch = (float)(outputs[0]);
        theta = (float)(outputs[1]);
        p_target = (float)(outputs[2]);
        InputConstraints.input_constraints(v_launch, theta, p_target);
        float t_flight = Calculations.func_t_flight(v_launch, theta, g_vect);
        float p_land = Calculations.func_p_land(v_launch, theta, g_vect);
        float d_offset = Calculations.func_d_offset(p_target, p_land);
        String s = Calculations.func_s(p_target, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}
