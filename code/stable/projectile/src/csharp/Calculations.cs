using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Calculations {
    
    public static double func_t_flight(InputParameters inParams) {
        return ((2 * (inParams.v_launch * Math.Sin(inParams.angle))) / 9.8);
    }
    
    public static double func_p_land(InputParameters inParams) {
        return ((2 * (Math.Pow(inParams.v_launch, 2) * (Math.Sin(inParams.angle) * Math.Cos(inParams.angle)))) / 9.8);
    }
    
    public static double func_d_offset(InputParameters inParams, double p_land) {
        return (p_land - inParams.p_target);
    }
    
    public static string func_s(InputParameters inParams, double d_offset) {
        if ((Math.Abs((d_offset / inParams.p_target)) < 2.0e-2)) {
            return "The target was hit.";
        }
        else if ((d_offset < 0)) {
            return "The projectile fell short.";
        }
        else if ((d_offset > 0)) {
            return "The projectile went long.";
        }
        else {
            throw new Exception("Undefined case encountered in function func_s");
        }
    }
}

