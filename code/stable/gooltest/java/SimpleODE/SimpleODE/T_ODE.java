package SimpleODE;

import java.util.ArrayList;
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations;

public class T_ODE implements FirstOrderDifferentialEquations {
    private double c;
    
    public T_ODE(double c) {
        this.c = c;
    }
    
    public int getDimension() {
        return 1;
    }
    
    public void computeDerivatives(float t, float[] T, float[] dT) {
        dT[0] = T[0] + c;
    }
}
