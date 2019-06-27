using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
    public static void write_output(double T_W, double E_W) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("T_W = ");
        outputfile.WriteLine(inParams.T_W);
        outputfile.Write("E_W = ");
        outputfile.WriteLine(inParams.E_W);
        outputfile.Close();
    }
}

