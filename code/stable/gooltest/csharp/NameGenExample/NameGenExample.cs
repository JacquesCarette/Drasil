using System;
using System.Collections.Generic;

public class NameGenExample {
    
    public static void Main(string[] args) {
        List<int> temp = new List<int> {1, 2, 3};
        List<int> result = new List<int>(2);
        
        List<int> temp0 = new List<int>(0);
        for (int i = 1; i < 3; i++) {
            temp0.Add(temp[i]);
        }
        result = temp0;
        
        // This shadows a generated name:
        List<int> temp0 = new List<int> {1, 2, 3};
        // This shadows a user-given name:
        List<int> result = new List<int>(2);
    }
    
    private static void helper(List<int> temp) {
        List<int> result = new List<int>(2);
        
        List<int> temp0 = new List<int>(0);
        for (int i = 1; i < 3; i++) {
            temp0.Add(temp[i]);
        }
        result = temp0;
    }
}
