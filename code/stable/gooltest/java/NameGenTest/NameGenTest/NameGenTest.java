package NameGenTest;

import java.util.ArrayList;
import java.util.Arrays;

public class NameGenTest {
    
    public static void main(String[] args) {
        ArrayList<Integer> temp = new ArrayList<Integer>(Arrays.asList(1, 2, 3));
        ArrayList<Integer> result = new ArrayList<Integer>(2);
        
        ArrayList<Integer> temp0 = new ArrayList<Integer>(0);
        for (int i = 1; i < 3; i++) {
            temp0.add(temp.get(i));
        }
        result = temp0;
        
        assert result.size() == 2 : "Result list should have 2 elements after slicing.";
        
        assert result.get(0) == 2 : "First element of result should be 2.";
    }
    
    private static void helper(ArrayList<Integer> temp) {
        ArrayList<Integer> result = new ArrayList<Integer>(2);
        
        ArrayList<Integer> temp0 = new ArrayList<Integer>(0);
        for (int i = 1; i < 3; i++) {
            temp0.add(temp.get(i));
        }
        result = temp0;
        
        assert result.size() == 2 : "Result list should have 2 elements after slicing.";
    }
}
