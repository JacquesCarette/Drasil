using System;
using System.IO;

public class Projectile {
    
    public static void Main(string[] args) {
        string filename = args[0];
        double v_launch;
        double theta;
        double p_target;
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        get_input(filename, out v_launch, out theta, out p_target);
        input_constraints(v_launch, theta, p_target);
        double t_flight = func_t_flight(v_launch, theta, g_vect);
        double p_land = func_p_land(v_launch, theta, g_vect);
        double d_offset = func_d_offset(p_target, p_land);
        string s = func_s(p_target, epsilon, d_offset);
        write_output(s, d_offset);
    }
    
    public static double func_t_flight(double v_launch, double theta, double g_vect) {
        return 2 * v_launch * Math.Sin(theta) / g_vect;
    }
    
    public static double func_p_land(double v_launch, double theta, double g_vect) {
        return 2 * Math.Pow(v_launch, 2) * Math.Sin(theta) * Math.Cos(theta) / g_vect;
    }
    
    public static double func_d_offset(double p_target, double p_land) {
        return p_land - p_target;
    }
    
    public static string func_s(double p_target, double epsilon, double d_offset) {
        if (Math.Abs(d_offset / p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
    
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
    
    public static void input_constraints(double v_launch, double theta, double p_target) {
        if (!(v_launch > 0)) {
            Console.Write("Warning: ");
            Console.Write("v_launch has value ");
            Console.Write(v_launch);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
        if (!(0 < theta && theta < Math.PI / 2)) {
            Console.Write("Warning: ");
            Console.Write("theta has value ");
            Console.Write(theta);
            Console.Write(" but suggested to be ");
            Console.Write("between ");
            Console.Write(0);
            Console.Write(" and ");
            Console.Write(Math.PI / 2);
            Console.Write(" ((pi)/(2))");
            Console.WriteLine(".");
        }
        if (!(p_target > 0)) {
            Console.Write("Warning: ");
            Console.Write("p_target has value ");
            Console.Write(p_target);
            Console.Write(" but suggested to be ");
            Console.Write("above ");
            Console.Write(0);
            Console.WriteLine(".");
        }
    }
    
    public static void write_output(string s, double d_offset) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("s = ");
        outputfile.WriteLine(s);
        outputfile.Write("d_offset = ");
        outputfile.WriteLine(d_offset);
        outputfile.Close();
    }
}
