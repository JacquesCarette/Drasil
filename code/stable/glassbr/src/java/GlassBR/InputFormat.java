package GlassBR;

/** \file InputFormat.java
    \author Nikitha Krithnan and W. Spencer Smith
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
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function get_input called with inputs: {");
        outfile.print("  filename = ");
        outfile.print(filename);
        outfile.println(", ");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        inParams.a = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.a' assigned to ");
        outfile.print(inParams.a);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.b = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.b' assigned to ");
        outfile.print(inParams.b);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.w = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.w' assigned to ");
        outfile.print(inParams.w);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.P_btol = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.P_btol' assigned to ");
        outfile.print(inParams.P_btol);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.TNT = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.TNT' assigned to ");
        outfile.print(inParams.TNT);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.g = infile.nextLine();
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.g' assigned to ");
        outfile.print(inParams.g);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.t = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.t' assigned to ");
        outfile.print(inParams.t);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.SD_x = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.SD_x' assigned to ");
        outfile.print(inParams.SD_x);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.SD_y = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.SD_y' assigned to ");
        outfile.print(inParams.SD_y);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.nextLine();
        inParams.SD_z = Double.parseDouble(infile.nextLine());
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.print("var 'inParams.SD_z' assigned to ");
        outfile.print(inParams.SD_z);
        outfile.println(" in module InputFormat");
        outfile.close();
        infile.close();
    }
}

