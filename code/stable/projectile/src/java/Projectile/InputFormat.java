package Projectile;

/** \file InputFormat.java
    \brief Provides the function for reading inputs
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
        \param filename name of the input file
        \param inParams structure holding the input values
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

