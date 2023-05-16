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
        StreamWriter outfile;
        string filename = args[0];
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'filename' assigned ");
        outfile.Write(filename);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        InputParameters inParams = new InputParameters(filename);
        float t_flight = func_t_flight(inParams);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 't_flight' assigned ");
        outfile.Write(t_flight);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        float p_land = func_p_land(inParams);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'p_land' assigned ");
        outfile.Write(p_land);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        float d_offset = func_d_offset(inParams, p_land);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'd_offset' assigned ");
        outfile.Write(d_offset);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        string s = func_s(inParams, d_offset);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 's' assigned ");
        outfile.Write(s);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        write_output(s, d_offset, t_flight);
    }
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param inParams structure holding the input values
        \return flight duration: the time when the projectile lands (s)
    */
    public static float func_t_flight(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_t_flight called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 2.0f * inParams.v_launch * (float)(Math.Sin(inParams.theta)) / inParams.g;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param inParams structure holding the input values
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static float func_p_land(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_p_land called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return 2.0f * (float)(Math.Pow(inParams.v_launch, 2.0f)) * (float)(Math.Sin(inParams.theta)) * (float)(Math.Cos(inParams.theta)) / inParams.g;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param inParams structure holding the input values
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static float func_d_offset(InputParameters inParams, float p_land) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_d_offset called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  p_land = ");
        outfile.WriteLine(p_land);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return p_land - inParams.p_target;
    }
    
    /** \brief Calculates output message as a string
        \param inParams structure holding the input values
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static string func_s(InputParameters inParams, float d_offset) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_s called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  d_offset = ");
        outfile.WriteLine(d_offset);
        outfile.WriteLine("  }");
        outfile.Close();
        
        if (Math.Abs(d_offset / inParams.p_target) < inParams.epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0.0f) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
    
    /** \brief Writes the output values to output.txt
        \param s output message as a string
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param t_flight flight duration: the time when the projectile lands (s)
    */
    public static void write_output(string s, float d_offset, float t_flight) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function write_output called with inputs: {");
        outfile.Write("  s = ");
        outfile.Write(s);
        outfile.WriteLine(", ");
        outfile.Write("  d_offset = ");
        outfile.Write(d_offset);
        outfile.WriteLine(", ");
        outfile.Write("  t_flight = ");
        outfile.WriteLine(t_flight);
        outfile.WriteLine("  }");
        outfile.Close();
        
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

/** \brief Structure for holding the input values and constant values
*/
public class InputParameters {
    public float v_launch;
    public float theta;
    public float p_target;
    public float g = 9.8f;
    public float epsilon = 2.0e-2f;
    
    /** \brief Initializes input object by reading inputs and checking physical constraints on the input
        \param filename name of the input file
    */
    public InputParameters(string filename) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function InputParameters called with inputs: {");
        outfile.Write("  filename = ");
        outfile.WriteLine(filename);
        outfile.WriteLine("  }");
        outfile.Close();
        
        this.get_input(filename);
        this.input_constraints();
    }
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
    */
    private void get_input(string filename) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function get_input called with inputs: {");
        outfile.Write("  filename = ");
        outfile.WriteLine(filename);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        this.v_launch = Single.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.v_launch' assigned ");
        outfile.Write(this.v_launch);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        infile.ReadLine();
        this.theta = Single.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.theta' assigned ");
        outfile.Write(this.theta);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        infile.ReadLine();
        this.p_target = Single.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.p_target' assigned ");
        outfile.Write(this.p_target);
        outfile.WriteLine(" in module Projectile");
        outfile.Close();
        infile.Close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
    */
    private void input_constraints() {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function input_constraints called with inputs: {");
        outfile.WriteLine("  }");
        outfile.Close();
        
        if (!(this.v_launch > 0.0f)) {
            Console.Write("Warning: ");
            Console.Write("v_launch has value ");
            Console.Write(this.v_launch);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0f);
            Console.WriteLine(".");
        }
        if (!(0.0f < this.theta && this.theta < Math.PI / 2.0f)) {
            Console.Write("Warning: ");
            Console.Write("theta has value ");
            Console.Write(this.theta);
            Console.Write(", but is suggested to be ");
            Console.Write("between ");
            Console.Write(0.0f);
            Console.Write(" and ");
            Console.Write(Math.PI / 2.0f);
            Console.Write(" ((pi)/(2))");
            Console.WriteLine(".");
        }
        if (!(this.p_target > 0.0f)) {
            Console.Write("Warning: ");
            Console.Write("p_target has value ");
            Console.Write(this.p_target);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0f);
            Console.WriteLine(".");
        }
    }
}
