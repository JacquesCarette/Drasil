package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Calculations {
    
    public static double func_q(InputParameters inParams) throws Exception {
        return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
    
    public static Boolean func_is_safePb(InputParameters inParams, double P_b) throws Exception {
        return P_b < inParams.P_btol;
    }
    
    public static Boolean func_is_safeLR(double LR, double q) throws Exception {
        return LR > q;
    }
    
    public static double func_B(InputParameters inParams, double J) throws Exception {
        return (2.86e-53 / (Math.pow(inParams.a * inParams.b, 7.0 - 1))) * ((Math.pow(7.17e10 * (Math.pow(inParams.h, 2)), 7.0)) * (inParams.LDF * (Math.exp(J))));
    }
    
    public static double func_J(InputParameters inParams, double q_hat) throws Exception {
        return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat);
    }
    
    public static double func_NFL(InputParameters inParams, double q_hat_tol) throws Exception {
        return (q_hat_tol * (7.17e10 * (Math.pow(inParams.h, 4)))) / (Math.pow(inParams.a * inParams.b, 2));
    }
    
    public static double func_q_hat(InputParameters inParams, double q) throws Exception {
        return (q * (Math.pow(inParams.a * inParams.b, 2))) / (7.17e10 * ((Math.pow(inParams.h, 4)) * inParams.GTF));
    }
    
    public static double func_q_hat_tol(InputParameters inParams, double J_tol) throws Exception {
        return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol);
    }
    
    public static double func_J_tol(InputParameters inParams) throws Exception {
        return Math.log((Math.log(1 / (1 - inParams.P_btol))) * ((Math.pow(inParams.a * inParams.b, 7.0 - 1)) / (2.86e-53 * ((Math.pow(7.17e10 * (Math.pow(inParams.h, 2)), 7.0)) * inParams.LDF))));
    }
    
    public static double func_P_b(double B) throws Exception {
        return 1 - (Math.exp(-(B)));
    }
    
    public static double func_LR(InputParameters inParams, double NFL) throws Exception {
        return NFL * (inParams.GTF * 1);
    }
    
    public static double func_q(InputParameters inParams) throws Exception {
        return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
}

