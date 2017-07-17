using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR_program {
    public class InputParameters {
        public static double a;
        public static double b;
        public static double w;
        public static double SD;
        public static double P_btol;
        public static int TNT;
        public static string g;
        public static double t;
        
        public static void get_inputs(string filename) {
            StreamReader infile;
            infile = new StreamReader(filename);
            a = Double.Parse(infile.ReadLine());
            b = Double.Parse(infile.ReadLine());
            w = Double.Parse(infile.ReadLine());
            SD = Double.Parse(infile.ReadLine());
            P_btol = Double.Parse(infile.ReadLine());
            TNT = Int32.Parse(infile.ReadLine());
            g = (infile.ReadLine());
            t = Double.Parse(infile.ReadLine());
            infile.Close();
        }
        
        public static void input_constraints() {
            if (!(d_min <= a)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(a <= d_max)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!((a / b) < AR_max)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(d_min <= b)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(b <= d_max)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!((a / b) < AR_max)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(w_max <= w)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(w <= w_min)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(SD_min < SD)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(SD < SD_max)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(a > 0.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!((a / b) > 1.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(b > 0.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(b < a)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(w >= 0.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(SD > 0.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(0.0 < P_btol)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(P_btol < 1.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
            if (!(TNT > 0.0)) {
                Console.WriteLine("Warning: constraint violated");
            }
        }
    }
}

