using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

public class Calculations {
    
    public static double func_J_tol(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J_tol called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Math.Log((Math.Log((1 / (1 - inParams.P_btol))) * (Math.Pow((inParams.a * inParams.b), (7.0 - 1)) / (2.86e-53 * (Math.Pow((7.17e10 * Math.Pow(inParams.h, 2)), 7.0) * inParams.LDF)))));
    }
    
    public static double func_q(InputParameters inParams) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.WriteLine("Instance of InputParameters object");
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.func_interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
    
    public static double func_q_hat(InputParameters inParams, double q) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q = ");
        outfile.WriteLine(q);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return ((q * Math.Pow((inParams.a * inParams.b), 2)) / (7.17e10 * (Math.Pow(inParams.h, 4) * inParams.GTF)));
    }
    
    public static double func_q_hat_tol(InputParameters inParams, double J_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_q_hat_tol called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  J_tol = ");
        outfile.WriteLine(J_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.func_interpY("SDF.txt", inParams.AR, J_tol);
    }
    
    public static double func_J(InputParameters inParams, double q_hat) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_J called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q_hat = ");
        outfile.WriteLine(q_hat);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return Interpolation.func_interpZ("SDF.txt", inParams.AR, q_hat);
    }
    
    public static double func_NFL(InputParameters inParams, double q_hat_tol) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_NFL called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  q_hat_tol = ");
        outfile.WriteLine(q_hat_tol);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return ((q_hat_tol * (7.17e10 * Math.Pow(inParams.h, 4))) / Math.Pow((inParams.a * inParams.b), 2));
    }
    
    public static double func_B(InputParameters inParams, double J) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_B called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  J = ");
        outfile.WriteLine(J);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return ((2.86e-53 / Math.Pow((inParams.a * inParams.b), (7.0 - 1))) * (Math.Pow((7.17e10 * Math.Pow(inParams.h, 2)), 7.0) * (inParams.LDF * Math.Exp(J))));
    }
    
    public static double func_LR(InputParameters inParams, double NFL) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_LR called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  NFL = ");
        outfile.WriteLine(NFL);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return (NFL * (inParams.GTF * 1));
    }
    
    public static Boolean func_is_safeLR(double LR, double q) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_is_safeLR called with inputs: {");
        outfile.Write("  LR = ");
        outfile.Write(LR);
        outfile.WriteLine(", ");
        outfile.Write("  q = ");
        outfile.WriteLine(q);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return (LR > q);
    }
    
    public static double func_P_b(double B) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_P_b called with inputs: {");
        outfile.Write("  B = ");
        outfile.WriteLine(B);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return (1 - Math.Exp(-(B)));
    }
    
    public static Boolean func_is_safePb(InputParameters inParams, double P_b) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function func_is_safePb called with inputs: {");
        outfile.Write("  inParams = ");
        outfile.Write("Instance of InputParameters object");
        outfile.WriteLine(", ");
        outfile.Write("  P_b = ");
        outfile.WriteLine(P_b);
        outfile.WriteLine("  }");
        outfile.Close();
        
        return (P_b < inParams.P_btol);
    }
}

