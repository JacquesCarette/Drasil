using System;
using System.Collections.Generic;
using System.Diagnostics;

public class NameGenTest {
    
    public static void Main(string[] args) {
        List<int> temp = new List<int> {1, 2, 3};
        List<int> result = new List<int>(2);
        
        List<int> temp0 = new List<int>(0);
        for (int i = 1; i < 3; i++) {
            temp0.Add(temp[i]);
        }
        result = temp0;
        
        Debug.Assert( result.Count == 2 , "Result list should have 2 elements after slicing.");
        
        Debug.Assert( result[0] == 2 , "First element of result should be 2.");
    }
    
    private static void helper(List<int> temp) {
        List<int> result = new List<int>(2);
        
        List<int> temp0 = new List<int>(0);
        for (int i = 1; i < 3; i++) {
            temp0.Add(temp[i]);
        }
        result = temp0;
        
        Debug.Assert( result.Count == 2 , "Result list should have 2 elements after slicing.");
    }
}
