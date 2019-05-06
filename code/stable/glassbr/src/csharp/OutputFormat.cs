using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class OutputFormat {
        
        public static void write_output(Boolean is_safePb, Boolean is_safeLR, double P_b) {
            StreamWriter outfile;
            outfile = new StreamWriter("output.txt");
            outfile.Write("is_safePb = ");
            outfile.WriteLine(is_safePb);
            outfile.Write("is_safeLR = ");
            outfile.WriteLine(is_safeLR);
            outfile.Write("P_b = ");
            outfile.WriteLine(P_b);
            outfile.Close();
        }
    }
}

