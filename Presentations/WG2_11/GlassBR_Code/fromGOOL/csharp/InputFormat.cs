using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputFormat {
        
        public static void get_input(string filename, InputParameters inparams) {
            StreamReader infile;
            infile = new StreamReader(filename);
            inparams.a = Double.Parse(infile.ReadLine());
            inparams.b = Double.Parse(infile.ReadLine());
            inparams.t = Double.Parse(infile.ReadLine());
            inparams.gt = Int32.Parse(infile.ReadLine());
            inparams.w = Double.Parse(infile.ReadLine());
            inparams.tnt = Double.Parse(infile.ReadLine());
            inparams.sdx = Double.Parse(infile.ReadLine());
            inparams.sdy = Double.Parse(infile.ReadLine());
            inparams.sdz = Double.Parse(infile.ReadLine());
            inparams.pbtol = Double.Parse(infile.ReadLine());
            infile.Close();
        }
    }
}

