using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace SWHS {
    public class Calculations {
        
        public static double func_q_C(InputParameters inParams, double t) {
            return inParams.h_C * (inParams.T_C - func_T_W(t));
        }
        
        public static double func_q_C(InputParameters inParams, double t) {
            return inParams.h_C * (inParams.T_C - func_T_W(t));
        }
    }
}

