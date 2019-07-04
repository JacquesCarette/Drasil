package Projectile;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Scanner;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.File;
import java.util.ArrayList;

public class Calculations {
    
    public static double func_t_flight(InputParameters inParams) throws Exception {
        return ((2 * (inParams.v_launch * Math.sin(inParams.angle))) / 9.8);
    }
    
    public static double func_p_land(InputParameters inParams) throws Exception {
        return ((2 * (Math.pow(inParams.v_launch, 2) * (Math.sin(inParams.angle) * Math.cos(inParams.angle)))) / 9.8);
    }
    
    public static double func_d_offset(InputParameters inParams, double p_land) throws Exception {
        return (p_land - inParams.p_target);
    }
    
    public static String func_s(InputParameters inParams, double d_offset) throws Exception {
        if ((Math.abs((d_offset / inParams.p_target)) < 2.0e-2)) {
            return "The target was hit.";
        }
        else if ((d_offset < 0)) {
            return "The projectile fell short.";
        }
        else if ((d_offset > 0)) {
            return "The projectile went long.";
        }
    }
}

