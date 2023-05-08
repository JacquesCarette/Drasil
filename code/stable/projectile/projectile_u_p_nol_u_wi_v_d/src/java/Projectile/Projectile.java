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
        String filename = args[0];
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        double v_launch;
        double theta;
        double p_target;
        Object[] outputs = get_input(filename);
        v_launch = (double)(outputs[0]);
        theta = (double)(outputs[1]);
        p_target = (double)(outputs[2]);
        input_constraints(v_launch, theta, p_target);
        double t_flight = func_t_flight(v_launch, theta, g_vect);
        double p_land = func_p_land(v_launch, theta, g_vect);
        double d_offset = func_d_offset(p_target, p_land);
        String s = func_s(p_target, epsilon, d_offset);
        write_output(s, d_offset, t_flight);
    }
    
    /** \brief Calculates flight duration: the time when the projectile lands (s)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return flight duration: the time when the projectile lands (s)
    */
    public static double func_t_flight(double v_launch, double theta, double g_vect) {
        return 2.0 * v_launch * Math.sin(theta) / g_vect;
    }
    
    /** \brief Calculates landing position: the distance from the launcher to the final position of the projectile (m)
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param g_vect gravitational acceleration (m/s^2)
        \return landing position: the distance from the launcher to the final position of the projectile (m)
    */
    public static double func_p_land(double v_launch, double theta, double g_vect) {
        return 2.0 * Math.pow(v_launch, 2.0) * Math.sin(theta) * Math.cos(theta) / g_vect;
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
    public static String func_s(double p_target, double epsilon, double d_offset) {
        if (Math.abs(d_offset / p_target) < epsilon) {
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
        \return array containing the following values:
        \return launch speed: the initial speed of the projectile when launched (m/s)
        \return launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \return target position: the distance from the launcher to the target (m)
    */
    public static Object[] get_input(String filename) throws FileNotFoundException {
        double v_launch;
        double theta;
        double p_target;
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        v_launch = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        theta = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        p_target = Double.parseDouble(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[3];
        outputs[0] = v_launch;
        outputs[1] = theta;
        outputs[2] = p_target;
        return outputs;
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param v_launch launch speed: the initial speed of the projectile when launched (m/s)
        \param theta launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
        \param p_target target position: the distance from the launcher to the target (m)
    */
    public static void input_constraints(double v_launch, double theta, double p_target) {
        if (!(v_launch > 0.0)) {
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(v_launch);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
        }
        if (!(0.0 < theta && theta < Math.PI / 2.0)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(theta);
            System.out.print(", but is suggested to be ");
            System.out.print("between ");
            System.out.print(0.0);
            System.out.print(" and ");
            System.out.print(Math.PI / 2.0);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(p_target > 0.0)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(p_target);
            System.out.print(", but is suggested to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
        }
    }
    
    /** \brief Writes the output values to output.txt
        \param s output message as a string
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
        \param t_flight flight duration: the time when the projectile lands (s)
    */
    public static void write_output(String s, double d_offset, double t_flight) throws IOException {
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
