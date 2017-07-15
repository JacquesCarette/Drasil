package GlassBR;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.File;
import java.util.Vector;

public class Calculations {
    
    public static double calc_q_hat(double q, InputParameters inparams) {
        double q_hat = ((q * (Math.pow(inparams.a * inparams.b, 2.0))) / (inparams.E * (Math.pow(inparams.h, 4.0)))) * (1.0 / inparams.gtf);
        return q_hat;
    }
    
    public static double calc_j_tol(InputParameters inparams) {
        double j_tol = Math.log((Math.log(1.0 / (1.0 - inparams.pbtol))) * ((Math.pow((inparams.a / 1000.0) * (inparams.b / 1000.0), inparams.m - 1.0)) / ((inparams.k * (Math.pow((inparams.E * 1000.0) * (Math.pow(inparams.h / 1000.0, 2.0)), inparams.m))) * inparams.ldf)));
        return j_tol;
    }
    
    public static double calc_pb(double j, InputParameters inparams) {
        double b = (((inparams.k / (Math.pow(((inparams.a / 1000.0) * inparams.b) / 1000.0, inparams.m - 1.0))) * (Math.pow((1000.0 * inparams.E) * (Math.pow(inparams.h / 1000.0, 2.0)), inparams.m))) * inparams.ldf) * (Math.exp(j));
        double pb = 1.0 - (Math.exp(-(b)));
        return pb;
    }
    
    public static double calc_nfl(double q_hat_tol, InputParameters inparams) {
        double nfl = ((q_hat_tol * inparams.E) * (Math.pow(inparams.h, 4.0))) / (Math.pow(inparams.a * inparams.b, 2.0));
        return nfl;
    }
    
    public static double calc_lr(double nfl, InputParameters inparams) {
        double lr = (nfl * inparams.gtf) * inparams.lsf;
        return lr;
    }
    
    public static Boolean calc_is_safe1(double pb, InputParameters inparams) {
        Boolean is_safe1;
        if (pb < inparams.pbtol) {
            is_safe1 = true;
        }
        else {
            is_safe1 = false;
        }
        return is_safe1;
    }
    
    public static Boolean calc_is_safe2(double lr, double q) {
        Boolean is_safe2;
        if (lr > q) {
            is_safe2 = true;
        }
        else {
            is_safe2 = false;
        }
        return is_safe2;
    }
}

