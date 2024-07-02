package VectorTest;

import java.util.ArrayList;
import java.util.Arrays;

public class VectorTest {
    
    public static void main(String[] args) {
        ArrayList<Double> v1 = new ArrayList<Double>(Arrays.asList(1.0, 1.5));
        ArrayList<Double> v2 = new ArrayList<Double>(Arrays.asList(0.0, -1.0));
        for (int i = 0; i < v1.size(); i += 1) {
            v1.set(i, 2.0 * v1.get(i) + v2.get(i));
        }
        double x;
        x = 0.0;
        for (int j = 0; j < v1.size(); j += 1) {
            x += v1.get(j) * v2.get(j);
        }
    }
}
