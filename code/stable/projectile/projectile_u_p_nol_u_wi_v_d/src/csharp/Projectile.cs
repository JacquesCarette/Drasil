/** \file Projectile.cs
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Contains the entire Projectile program
*/
using System;
using System.IO;

public class Projectile {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        double v_launch;
        double theta;
        double p_target;
        get_input(filename, out v_launch, out theta, out p_target);
        input_constraints(v_launch, theta, p_target);
        double t_flight = func_t_flight(v_launch, theta, g_vect);
        double p_land = func_p_land(v_launch, theta, g_vect);
        double d_offset = func_d_offset(p_target, p_land);
        string s = func_s(p_target, epsilon, d_offset);
        write_output(s, d_offset, t_flight);
    }
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(double v_launch, double theta, double g_vect) {
        return 2.0 * v_launch * Math.Sin(theta) / g_vect;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(double v_launch, double theta, double g_vect) {
        return 2.0 * Math.Pow(v_launch, 2.0) * Math.Sin(theta) * Math.Cos(theta) / g_vect;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param p_target target position: the distance from the launcher to the target (m)
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static double func_d_offset(double p_target, double p_land) {
        return p_land - p_target;
    }
    
    /** \brief Calculates output message as a string
        \param p_target target position: the distance from the launcher to the target (m)
        \param epsilon hit tolerance
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static string func_s(double p_target, double epsilon, double d_offset) {
        if (Math.Abs(d_offset / p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0.0) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void get_input(string filename, out double v_launch, out double theta, out double p_target) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        v_launch = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        theta = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        p_target = Double.Parse(infile.ReadLine());
        infile.Close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void input_constraints(double v_launch, double theta, double p_target) {
        if (!(v_launch > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("v_launch has value ");
            Console.Write(v_launch);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
        if (!(0.0 < theta && theta < Math.PI / 2.0)) {
            Console.Write("Warning: ");
            Console.Write("theta has value ");
            Console.Write(theta);
            Console.Write(", but is suggested to be ");
            Console.Write("between ");
            Console.Write(0.0);
            Console.Write(" and ");
            Console.Write(Math.PI / 2.0);
            Console.Write(" ((pi)/(2))");
            Console.WriteLine(".");
        }
        if (!(p_target > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("p_target has value ");
            Console.Write(p_target);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
    }
    
    /** \brief Writes the output values to output.txt
        \param s output message as a string
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param t_flight flight duration: the time when the projectile lands (s)
    */
    public static void write_output(string s, double d_offset, double t_flight) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("s = ");
        outputfile.WriteLine(s);
        outputfile.Write("d_offset = ");
        outputfile.WriteLine(d_offset);
        outputfile.Write("t_flight = ");
        outputfile.WriteLine(t_flight);
        outputfile.Close();
    }
}
