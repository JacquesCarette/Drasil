package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class InputFormat {
    
    public static void get_input(String filename, InputParameters inparams) throws Exception {
        Scanner infile;
        infile = new Scanner(new File(filename));
        inparams.a = infile.nextDouble();
        inparams.b = infile.nextDouble();
        inparams.t = infile.nextDouble();
        inparams.gt = infile.nextInt();
        inparams.w = infile.nextDouble();
        inparams.tnt = infile.nextDouble();
        inparams.sdx = infile.nextDouble();
        inparams.sdy = infile.nextDouble();
        inparams.sdz = infile.nextDouble();
        inparams.pbtol = infile.nextDouble();
        infile.close();
    }
}

