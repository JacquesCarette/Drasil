package SimpleODE;

import java.util.ArrayList;
import org.apache.commons.math3.ode.nonstiff.DormandPrince54Integrator;

public class SimpleODE {
    
    public static void main(String[] args) {
        double c = 3.5;
        
        DormandPrince54Integrator it = new DormandPrince54Integrator(1.0, 1.0, 1.0e-3, 1.0e-3);
        T_ODE ode = new T_ODE(c);
        float[] T_ode = {1.0};
        ArrayList<Double> T;
        
        T_StepHandler stepHandler = new T_StepHandler();
        it.addStepHandler(stepHandler);
        it.integrate(ode, 0.0, T_ode, 10.0, T_ode);
        T = stepHandler.T;
        
        System.out.print(T);
    }
}
