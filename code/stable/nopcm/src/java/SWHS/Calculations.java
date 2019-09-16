package SWHS;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Calculations {
    
    public static double func_q_C(InputParameters inParams, double t) throws Exception {
        return inParams.h_C * (inParams.T_C - func_T_W(t));
    }
    
    public static double func_q_C(InputParameters inParams, double t) throws Exception {
        return inParams.h_C * (inParams.T_C - func_T_W(t));
    }
}

