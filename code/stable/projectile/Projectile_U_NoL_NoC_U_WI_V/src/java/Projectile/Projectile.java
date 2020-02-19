package Projectile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;

public class Projectile {
    
    public static void main(String[] args) throws FileNotFoundException, IOException {
        String filename = args[0];
        double v_launch;
        double theta;
        double p_target;
        double g_vect = 9.8;
        double epsilon = 2.0e-2;
        Object[] outputs = get_input(filename);
        v_launch = (double)(outputs[0]);
        theta = (double)(outputs[1]);
        p_target = (double)(outputs[2]);
        input_constraints(v_launch, theta, p_target);
        double t_flight = func_t_flight(v_launch, theta, g_vect);
        double p_land = func_p_land(v_launch, theta, g_vect);
        double d_offset = func_d_offset(p_target, p_land);
        String s = func_s(p_target, epsilon, d_offset);
        write_output(s, d_offset);
    }
    
    public static double func_t_flight(double v_launch, double theta, double g_vect) {
        return 2 * v_launch * Math.sin(theta) / g_vect;
    }
    
    public static double func_p_land(double v_launch, double theta, double g_vect) {
        return 2 * Math.pow(v_launch, 2) * Math.sin(theta) * Math.cos(theta) / g_vect;
    }
    
    public static double func_d_offset(double p_target, double p_land) {
        return p_land - p_target;
    }
    
    public static String func_s(double p_target, double epsilon, double d_offset) {
        if (Math.abs(d_offset / p_target) < epsilon) {
            return "The target was hit.";
        }
        else if (d_offset < 0) {
            return "The projectile fell short.";
        }
        else {
            return "The projectile went long.";
        }
    }
    
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
    
    public static void input_constraints(double v_launch, double theta, double p_target) {
        if (!(v_launch > 0)) {
            System.out.print("Warning: ");
            System.out.print("v_launch has value ");
            System.out.print(v_launch);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
        if (!(0 < theta && theta < Math.PI / 2)) {
            System.out.print("Warning: ");
            System.out.print("theta has value ");
            System.out.print(theta);
            System.out.print(" but suggested to be ");
            System.out.print("between ");
            System.out.print(0);
            System.out.print(" and ");
            System.out.print(Math.PI / 2);
            System.out.print(" ((pi)/(2))");
            System.out.println(".");
        }
        if (!(p_target > 0)) {
            System.out.print("Warning: ");
            System.out.print("p_target has value ");
            System.out.print(p_target);
            System.out.print(" but suggested to be ");
            System.out.print("above ");
            System.out.print(0);
            System.out.println(".");
        }
    }
    
    public static void write_output(String s, double d_offset) throws IOException {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("s = ");
        outputfile.println(s);
        outputfile.print("d_offset = ");
        outputfile.println(d_offset);
        outputfile.close();
    }
}
