/** \file InputFormat.cs
    \brief Provides the function for reading inputs
*/
using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename No description given
    */
    public static void get_input(string filename, InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function get_input called with inputs: {");
        outfile.Write("  filename = ");
        outfile.Write(filename);
        outfile.WriteLine(", ");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamReader infile;
        string line;
        List<string> lines = new List<string>(0);
        List<string> linetokens = new List<string>(0);
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.a = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.b = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.w = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.P_btol = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.TNT = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.g = (infile.ReadLine());
        infile.ReadLine();
        inParams.t = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.SD_x = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.SD_y = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        inParams.SD_z = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}

