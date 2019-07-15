using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class OutputFormat {
    
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

