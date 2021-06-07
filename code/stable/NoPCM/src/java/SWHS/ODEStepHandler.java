package SWHS;

/** \file ODEStepHandler.java
    \author Thulasi Jegatheesan
    \brief Class defining additional behaviour for each step of an ODE solution
*/
import java.util.ArrayList;
import java.util.Arrays;
import org.apache.commons.math3.ode.sampling.StepHandler;
import org.apache.commons.math3.ode.sampling.StepInterpolator;

/** \brief Class defining additional behaviour for each step of an ODE solution
*/
public class ODEStepHandler implements StepHandler {
    public ArrayList<Double> T_W;
    
    /** \brief initializes step handler with initial conditions
        \param t0 initial time for ODE solving
        \param y0 array of initial values for ODE solving
        \param t current independent variable value in ODE solution
    */
    public void init(double t0, double[] y0, double t) {
        T_W = new ArrayList<Double>(Arrays.asList(y0[0]));
    }
    
    /** \brief appends solution point at each ODE solution step
        \param interpolator step interpolator for ODE solving
        \param isLast boolean for whether the current step is the last step
    */
    public void handleStep(StepInterpolator interpolator, boolean isLast) {
        double[] curr = interpolator.getInterpolatedState();
        T_W.add(curr[0]);
    }
}
