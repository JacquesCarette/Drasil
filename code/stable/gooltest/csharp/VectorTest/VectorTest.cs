using System;
using System.Collections.Generic;
using System.Diagnostics;

public class VectorTest {
    
    public static void Main(string[] args) {
        List<double> v1 = new List<double> {1.0, 1.5};
        List<double> v2 = new List<double> {0.0, -1.0};
        for (int i = 0; i < v1.Count; i += 1) {
            v1[i] = 2.0 * v1[i] + v2[i];
        }
        double x;
        x = 0.0;
        for (int j = 0; j < v1.Count; j += 1) {
            x += v1[j] * v2[j];
        }
        Debug.Assert( x == -2.0 , "Dot product of v1 and v2 should be -2.");
    }
}
