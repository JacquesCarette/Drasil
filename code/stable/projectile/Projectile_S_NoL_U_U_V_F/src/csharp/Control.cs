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
        float v_launch;
        float theta;
        float p_target;
        float g_vect = 9.8f;
        float epsilon = 2.0e-2f;
        InputFormat.get_input(filename, out v_launch, out theta, out p_target);
        InputConstraints.input_constraints(v_launch, theta, p_target);
        float t_flight = Calculations.func_t_flight(v_launch, theta, g_vect);
        float p_land = Calculations.func_p_land(v_launch, theta, g_vect);
        float d_offset = Calculations.func_d_offset(p_target, p_land);
        string s = Calculations.func_s(p_target, epsilon, d_offset);
        OutputFormat.write_output(s, d_offset);
    }
}
