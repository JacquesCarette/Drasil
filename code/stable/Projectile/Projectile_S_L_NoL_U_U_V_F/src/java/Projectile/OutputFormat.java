package Projectile;

/** \file OutputFormat.java
    \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param s output message as a string
        \param d_offset distance between the target position and the landing position: the offset between the target position and the landing position (m)
    */
    public static void write_output(String s, float d_offset) throws IOException {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("s = ");
        outputfile.println(s);
        outputfile.print("d_offset = ");
        outputfile.println(d_offset);
        outputfile.close();
    }
}
