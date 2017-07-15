package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class OutputFormat {
    
    public static void display_output(String filename, double q, double j, double q_hat_tol, double pb, double lr, double nfl, Boolean is_safe1, Boolean is_safe2, InputParameters inparams) throws Exception {
        PrintWriter outfile;
        outfile = new PrintWriter(filename);
        outfile.print("a           ");
        outfile.println(inparams.a);
        outfile.print("b           ");
        outfile.println(inparams.b);
        outfile.print("t           ");
        outfile.println(inparams.t);
        outfile.print("w           ");
        outfile.println(inparams.w);
        outfile.print("tnt         ");
        outfile.println(inparams.tnt);
        outfile.print("sdx         ");
        outfile.println(inparams.sdx);
        outfile.print("sdy         ");
        outfile.println(inparams.sdy);
        outfile.print("sdz         ");
        outfile.println(inparams.sdz);
        outfile.print("pbtol       ");
        outfile.println(inparams.pbtol);
        outfile.print("asprat      ");
        outfile.println(inparams.asprat);
        outfile.print("sd          ");
        outfile.println(inparams.sd);
        outfile.print("h           ");
        outfile.println(inparams.h);
        outfile.print("gtf         ");
        outfile.println(inparams.gtf);
        outfile.print("ldf         ");
        outfile.println(inparams.ldf);
        outfile.print("wtnt        ");
        outfile.println(inparams.wtnt);
        outfile.print("E           ");
        outfile.println(inparams.E);
        outfile.print("td          ");
        outfile.println(inparams.td);
        outfile.print("m           ");
        outfile.println(inparams.m);
        outfile.print("k           ");
        outfile.println(inparams.k);
        outfile.print("lsf         ");
        outfile.println(inparams.lsf);
        outfile.print("gt          ");
        outfile.println(inparams.gt);
        outfile.print("Demand (q)                      ");
        outfile.println(q);
        outfile.print("Stress Distr. Factor (j)        ");
        outfile.println(j);
        outfile.print("Tolerable Pressure (q_hat_tol)  ");
        outfile.println(q_hat_tol);
        outfile.print("Prob. of Breakage (pb)          ");
        outfile.println(pb);
        outfile.print("Capacity (lr)                   ");
        outfile.println(lr);
        outfile.print("Non-Factored Load (nfl)         ");
        outfile.println(nfl);
        outfile.print("Safety Req. 1 (is_safe1)        ");
        outfile.println(is_safe1);
        outfile.print("Safety Req. 2 (is_safe2)        ");
        outfile.println(is_safe2);
        if (is_safe1 && is_safe2) {
            outfile.println("For the given input parameters, the glass is considered safe.");
        }
        else {
            outfile.println("For the given input parameters, the glass is NOT considered safe.");
        }
        outfile.close();
    }
}

