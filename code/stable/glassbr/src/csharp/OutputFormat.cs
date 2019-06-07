using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
    public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("is_safePb = ");
        outputfile.WriteLine(is_safePb);
        outputfile.Write("is_safeLR = ");
        outputfile.WriteLine(is_safeLR);
        outputfile.Write("P_b = ");
        outputfile.WriteLine(P_b);
        outputfile.Close();
    }
}

