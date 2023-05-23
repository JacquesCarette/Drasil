package Projectile;

/** \file Projectile.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Contains the entire Projectile program
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Projectile {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws FileNotFoundException, IOException {
        PrintWriter outfile;
        String filename = args[0];
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'filename' assigned ");
        outfile.print(filename);
        outfile.println(" in module Projectile");
        outfile.close();
        InputParameters inParams = new InputParameters(filename);
        double t_flight = func_t_flight(inParams);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 't_flight' assigned ");
        outfile.print(t_flight);
        outfile.println(" in module Projectile");
        outfile.close();
        double p_land = func_p_land(inParams);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'p_land' assigned ");
        outfile.print(p_land);
        outfile.println(" in module Projectile");
        outfile.close();
        double d_offset = func_d_offset(inParams, p_land);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'd_offset' assigned ");
        outfile.print(d_offset);
        outfile.println(" in module Projectile");
        outfile.close();
        String s = func_s(inParams, d_offset);
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 's' assigned ");
        outfile.print(s);
        outfile.println(" in module Projectile");
        outfile.close();
        write_output(s, d_offset, t_flight);
    }
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param inParams structure holding the input values
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(InputParameters inParams) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_t_flight called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return 2.0 * inParams.v_launch * Math.sin(inParams.theta) / Constants.g;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param inParams structure holding the input values
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(InputParameters inParams) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_p_land called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return 2.0 * Math.pow(inParams.v_launch, 2.0) * Math.sin(inParams.theta) * Math.cos(inParams.theta) / Constants.g;
    }
    
    /** \brief Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param inParams structure holding the input values
        \param p_land landing position: the distance from the launcher to the final position of the projectile (m)
        \return distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static double func_d_offset(InputParameters inParams, double p_land) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_d_offset called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  p_land = ");
        outfile.println(p_land);
        outfile.println("  }");
        outfile.close();
        
        return p_land - inParams.p_target;
    }
    
    /** \brief Calculates output message as a string
        \param inParams structure holding the input values
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \return output message as a string
    */
    public static String func_s(InputParameters inParams, double d_offset) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_s called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  d_offset = ");
        outfile.println(d_offset);
        outfile.println("  }");
        outfile.close();
        
        if (Math.abs(d_offset / inParams.p_target) < Constants.epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0.0) {
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
    public static void write_output(String s, double d_offset, double t_flight) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function write_output called with inputs: {");
        outfile.print("  s = ");
        outfile.print(s);
        outfile.println(", ");
        outfile.print("  d_offset = ");
        outfile.print(d_offset);
        outfile.println(", ");
        outfile.print("  t_flight = ");
        outfile.println(t_flight);
        outfile.println("  }");
        outfile.close();
        
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("s = ");
        outputfile.println(s);
        outputfile.print("d_offset = ");
        outputfile.println(d_offset);
        outputfile.print("t_flight = ");
        outputfile.println(t_flight);
        outputfile.close();
    }
}

/** \brief Structure for holding the input values
*/
class InputParameters {
    public double v_launch;
    public double theta;
    public double p_target;
    
    /** \brief Initializes input object by reading inputs and checking physical constraints on the input
        \param filename name of the input file
    */
    public InputParameters(String filename) throws FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function InputParameters called with inputs: {");
        outfile.print("  filename = ");
        outfile.println(filename);
        outfile.println("  }");
        outfile.close();
        
        this.get_input(filename);
        this.input_constraints();
    }
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
    */
    private void get_input(String filename) throws FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function get_input called with inputs: {");
        outfile.print("  filename = ");
        outfile.println(filename);
        outfile.println("  }");
        outfile.close();
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        this.v_launch = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'this.v_launch' assigned ");
        outfile.print(this.v_launch);
        outfile.println(" in module Projectile");
        outfile.close();
        infile.nextLine();
        this.theta = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'this.theta' assigned ");
        outfile.print(this.theta);
        outfile.println(" in module Projectile");
        outfile.close();
        infile.nextLine();
        this.p_target = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'this.p_target' assigned ");
        outfile.print(this.p_target);
        outfile.println(" in module Projectile");
        outfile.close();
        infile.close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
    */
    private void input_constraints() throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function input_constraints called with inputs: {");
        outfile.println("  }");
        outfile.close();
        
        if (!(this.v_launch > 0.0)) {
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(this.v_launch);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
        }
        if (!(0.0 < this.theta && this.theta < Math.PI / 2.0)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(this.theta);
            System.out.print(", but is suggested to be ");
            System.out.print("between ");
            System.out.print(0.0);
            System.out.print(" and ");
            System.out.print(Math.PI / 2.0);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(this.p_target > 0.0)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(this.p_target);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
        }
    }
}

/** \brief Structure for holding the constant values
*/
class Constants {
    public static final double g = 9.8;
    public static final double epsilon = 2.0e-2;
    
}
