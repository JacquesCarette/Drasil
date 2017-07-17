using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR_program {
    public class OutputFormat {
        
        public static void write_output(string filename, Boolean is_safe1, Boolean is_safe2, double P_b) {
            StreamWriter outfile;
            outfile = new StreamWriter(filename);
            outfile.Write("is_safe1 = ");
            outfile.WriteLine(is_safe1);
            outfile.Write("is_safe2 = ");
            outfile.WriteLine(is_safe2);
            outfile.Write("P_b = ");
            outfile.WriteLine(P_b);
            outfile.Close();
        }
    }
}

