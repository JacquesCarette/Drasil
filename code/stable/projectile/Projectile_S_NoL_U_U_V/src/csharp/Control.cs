/** \file Control.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Controls the flow of the program
*/
public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        double v_launch;
        double theta;
        double p_target;
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        InputFormat.get_input(filename, out v_launch, out theta, out p_target);
        InputConstraints.input_constraints(v_launch, theta, p_target);
        double t_flight = Calculations.func_t_flight(v_launch, theta, g_vect);
        double p_land = Calculations.func_p_land(v_launch, theta, g_vect);
        double d_offset = Calculations.func_d_offset(p_target, p_land);
        string s = Calculations.func_s(p_target, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}
