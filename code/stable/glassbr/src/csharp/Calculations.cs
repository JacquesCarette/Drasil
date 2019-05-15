using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class Calculations {
        
        public static double func_q(InputParameters inParams) {
            return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
        }
        
        public static Boolean func_is_safePb(InputParameters inParams, double P_b) {
            return P_b < inParams.P_btol;
        }
        
        public static Boolean func_is_safeLR(double LR, double q) {
            return LR > q;
        }
        
        public static double func_B(InputParameters inParams, double J) {
            return (2.86e-53 / (Math.Pow(inParams.a * inParams.b, 7.0 - 1))) * ((Math.Pow(7.17e10 * (Math.Pow(inParams.h, 2)), 7.0)) * (inParams.LDF * (Math.Exp(J))));
        }
        
        public static double func_J(InputParameters inParams, double q_hat) {
            return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat);
        }
        
        public static double func_NFL(InputParameters inParams, double q_hat_tol) {
            return (q_hat_tol * (7.17e10 * (Math.Pow(inParams.h, 4)))) / (Math.Pow(inParams.a * inParams.b, 2));
        }
        
        public static double func_q_hat(InputParameters inParams, double q) {
            return (q * (Math.Pow(inParams.a * inParams.b, 2))) / (7.17e10 * ((Math.Pow(inParams.h, 4)) * inParams.GTF));
        }
        
        public static double func_q_hat_tol(InputParameters inParams, double J_tol) {
            return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol);
        }
        
        public static double func_J_tol(InputParameters inParams) {
            return Math.Log((Math.Log(1 / (1 - inParams.P_btol))) * ((Math.Pow(inParams.a * inParams.b, 7.0 - 1)) / (2.86e-53 * ((Math.Pow(7.17e10 * (Math.Pow(inParams.h, 2)), 7.0)) * inParams.LDF))));
        }
        
        public static double func_P_b(double B) {
            return 1 - (Math.Exp(-(B)));
        }
        
        public static double func_LR(InputParameters inParams, double NFL) {
            return NFL * (inParams.GTF * 1);
        }
        
        public static double func_q(InputParameters inParams) {
            return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
        }
    }
}

