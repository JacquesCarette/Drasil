using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputFormat {
        
        public static void func_get_input(string filename, InputParameters inParams) {
            StreamReader infile;
            string line;
            List<string> lines = new List<string>(0);
            List<string> linetokens = new List<string>(0);
            infile = new StreamReader(filename);
            infile.ReadLine();
            inParams.a = Double.Parse(infile.ReadLine());
            inParams.b = Double.Parse(infile.ReadLine());
            inParams.t = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.g = (infile.ReadLine());
            infile.ReadLine();
            inParams.w = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.TNT = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.SD_x = Double.Parse(infile.ReadLine());
            inParams.SD_y = Double.Parse(infile.ReadLine());
            inParams.SD_z = Double.Parse(infile.ReadLine());
            infile.ReadLine();
            inParams.P_btol = Double.Parse(infile.ReadLine());
            infile.Close();
        }
    }
}

