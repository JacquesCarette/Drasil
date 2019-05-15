using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace SWHS {
    public class OutputFormat {
        
        public static void write_output(InputParameters inParams) {
            StreamWriter outfile;
            outfile = new StreamWriter("output.txt");
            outfile.Write("T_W = ");
            outfile.WriteLine(inParams.T_W);
            outfile.Write("E_W = ");
            outfile.WriteLine(inParams.E_W);
            outfile.Close();
        }
    }
}

