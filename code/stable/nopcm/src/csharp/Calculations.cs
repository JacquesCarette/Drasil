using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Calculations {
    
    public static double func_q_C(double h_C, double T_C, double t) {
        return (inParams.h_C * (inParams.T_C - func_T_W(t)));
    }
}

