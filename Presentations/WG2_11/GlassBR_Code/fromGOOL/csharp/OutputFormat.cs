using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class OutputFormat {
        
        public static void display_output(string filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, Boolean is_safe1, Boolean is_safe2, InputParameters inparams) {
            StreamWriter outfile;
            outfile = new StreamWriter(filename);
            outfile.Write("a           ");
            outfile.WriteLine(inparams.a);
            outfile.Write("b           ");
            outfile.WriteLine(inparams.b);
            outfile.Write("t           ");
            outfile.WriteLine(inparams.t);
            outfile.Write("w           ");
            outfile.WriteLine(inparams.w);
            outfile.Write("tnt         ");
            outfile.WriteLine(inparams.tnt);
            outfile.Write("sdx         ");
            outfile.WriteLine(inparams.sdx);
            outfile.Write("sdy         ");
            outfile.WriteLine(inparams.sdy);
            outfile.Write("sdz         ");
            outfile.WriteLine(inparams.sdz);
            outfile.Write("pbtol       ");
            outfile.WriteLine(inparams.pbtol);
            outfile.Write("asprat      ");
            outfile.WriteLine(inparams.asprat);
            outfile.Write("sd          ");
            outfile.WriteLine(inparams.sd);
            outfile.Write("h           ");
            outfile.WriteLine(inparams.h);
            outfile.Write("gtf         ");
            outfile.WriteLine(inparams.gtf);
            outfile.Write("ldf         ");
            outfile.WriteLine(inparams.ldf);
            outfile.Write("wtnt        ");
            outfile.WriteLine(inparams.wtnt);
            outfile.Write("E           ");
            outfile.WriteLine(inparams.E);
            outfile.Write("td          ");
            outfile.WriteLine(inparams.td);
            outfile.Write("m           ");
            outfile.WriteLine(inparams.m);
            outfile.Write("k           ");
            outfile.WriteLine(inparams.k);
            outfile.Write("lsf         ");
            outfile.WriteLine(inparams.lsf);
            outfile.Write("gt          ");
            outfile.WriteLine(inparams.gt);
            outfile.Write("Demand (q)                      ");
            outfile.WriteLine(q);
            outfile.Write("Stress Distr. Factor (j)        ");
            outfile.WriteLine(j);
            outfile.Write("Tolerable Pressure (q_hat_tol)  ");
            outfile.WriteLine(q_hat_tol);
            outfile.Write("Prob. of Breakage (pb)          ");
            outfile.WriteLine(pb);
            outfile.Write("Capacity (lr)                   ");
            outfile.WriteLine(lr);
            outfile.Write("Non-Factored Load (nfl)         ");
            outfile.WriteLine(nfl);
            outfile.Write("Safety Req. 1 (is_safe1)        ");
            outfile.WriteLine(is_safe1);
            outfile.Write("Safety Req. 2 (is_safe2)        ");
            outfile.WriteLine(is_safe2);
            if (is_safe1 && is_safe2) {
                outfile.WriteLine("For the given input parameters, the glass is considered safe.");
            }
            else {
                outfile.WriteLine("For the given input parameters, the glass is NOT considered safe.");
            }
            outfile.Close();
        }
    }
}

