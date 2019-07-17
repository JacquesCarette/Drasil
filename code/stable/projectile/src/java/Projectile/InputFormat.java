package Projectile;

/** \file InputFormat.java
*/
import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename No description given
        \param inParams No description given
    */
    public static void get_input(String filename, InputParameters inParams) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.v_launch = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.theta = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        inParams.p_target = Double.parseDouble(infile.nextLine());
        infile.close();
    }
}

