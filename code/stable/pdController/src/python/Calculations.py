## \file Calculations.py
# \author Naveen Ganesh Muralidharan
# \brief Provides functions for calculating the outputs
import numpy
import scipy.integrate

## \brief Calculates Process Variable: The output value from the power plant
# \param K_d Derivative Gain: Gain constant of the derivative controller
# \param K_p Proportional Gain: Gain constant of the proportional controller
# \param r_t Set-Point: The desired value that the control system must reach. This also knows as the reference variable
# \param t_sim Simulation Time: Total execution time of the PD simulation (s)
# \param t_step Step Time: Simulation step time (s)
# \return Process Variable: The output value from the power plant
def func_y_t(K_d, K_p, r_t, t_sim, t_step):
    def f(y_t, t):
        return [y_t[1], -(1.0 + K_d) * y_t[1] - (20.0 + K_p) * y_t[0] + r_t * K_p]
    
    x_axis = numpy.arange(0.0, t_sim, t_step)
    u_t = scipy.integrate.odeint(f, [0.0, 0.0], x_axis)
    y_t = u_t.T[0]
    
    return y_t
