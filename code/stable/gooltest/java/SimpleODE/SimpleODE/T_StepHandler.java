package SimpleODE;

import java.util.ArrayList;
import java.util.Arrays;
import org.apache.commons.math3.ode.sampling.StepHandler;
import org.apache.commons.math3.ode.sampling.StepInterpolator;

public class T_StepHandler implements StepHandler {
    public ArrayList<Double> T;
    
    public void init(float t0, float[] y0, float t) {
        this.T = new ArrayList<Double>(Arrays.asList(y0[0]));
    }
    
    public void handleStep(StepInterpolator interpolator, boolean isLast) {
        float[] T_curr = interpolator.getInterpolatedState();
        this.T.add(T_curr[0]);
    }
}
