/** \file InputFormat.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for reading inputs
*/
using System;
using System.IO;

public class InputFormat {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param inParams structure holding the input values
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
        infile = new StreamReader(filename);
        infile.ReadLine();
        inParams.a = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.a' assigned ");
        outfile.Write(inParams.a);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.b = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.b' assigned ");
        outfile.Write(inParams.b);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.w = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.w' assigned ");
        outfile.Write(inParams.w);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.P_btol = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.P_btol' assigned ");
        outfile.Write(inParams.P_btol);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.TNT = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.TNT' assigned ");
        outfile.Write(inParams.TNT);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.g = infile.ReadLine();
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.g' assigned ");
        outfile.Write(inParams.g);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.t = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.t' assigned ");
        outfile.Write(inParams.t);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.SD_x = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.SD_x' assigned ");
        outfile.Write(inParams.SD_x);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.SD_y = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.SD_y' assigned ");
        outfile.Write(inParams.SD_y);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.ReadLine();
        inParams.SD_z = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'inParams.SD_z' assigned ");
        outfile.Write(inParams.SD_z);
        outfile.WriteLine(" in module InputFormat");
        outfile.Close();
        infile.Close();
    }
}
