using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Research.Oslo;

public class SimpleODE {
    
    public static void Main(string[] args) {
        double c = 3.5;
        
        Options opts = new Options();
        opts.AbsoluteTolerance = 1.0e-3;
        opts.AbsoluteTolerance = 1.0e-3;
        
        Idrasierable<SolPoint> sol = Ode.RK547M(0.0, new Vector(1.0), (t, T) => new Vector(T[0] + c), opts);
        
        SolPoint[] points = sol.SolveFromToStep(0.0, 10.0, 1.0).ToArray();
        List<double> T = new List<double> {};
        foreach (SolPoint sp in points) {
            T.Add(sp.X);
        }
        
        Console.Write("[");
        for (int list_i1 = 0; list_i1 < T.Count - 1; list_i1++) {
            Console.Write(T[list_i1]);
            Console.Write(", ");
        }
        if (T.Count > 0) {
            Console.Write(T[T.Count - 1]);
        }
        Console.Write("]");
    }
}
