/** \file OutputFormat.cs
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param s output message as a string
        \param d_offset distance between the target position and the landing position (m)
    */
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

