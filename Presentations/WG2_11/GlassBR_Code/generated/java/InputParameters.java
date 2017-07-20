package GlassBR_program;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class InputParameters {
    public static double a;
    public static double b;
    public static double w;
    public static double SD;
    public static double P_btol;
    public static int TNT;
    public static String g;
    public static double t;
    
    public static void get_inputs(String filename) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        a = infile.nextDouble();
        b = infile.nextDouble();
        w = infile.nextDouble();
        SD = infile.nextDouble();
        P_btol = infile.nextDouble();
        TNT = infile.nextInt();
        g = infile.next();
        t = infile.nextDouble();
        infile.close();
    }
    
    public static void input_constraints() throws Exception {
        if (!(d_min <= a)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(a <= d_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((a / b) < AR_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(d_min <= b)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(b <= d_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((a / b) < AR_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(w_max <= w)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(w <= w_min)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(SD_min < SD)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(SD < SD_max)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(a > 0.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!((a / b) > 1.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(b > 0.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(b < a)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(w >= 0.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(SD > 0.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(0.0 < P_btol)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(P_btol < 1.0)) {
            System.out.println("Warning: constraint violated");
        }
        if (!(TNT > 0.0)) {
            System.out.println("Warning: constraint violated");
        }
    }
}

