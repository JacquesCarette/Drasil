import scipy.integrate

import scipy.integrate

c = 3.5

r = scipy.integrate.ode(lambda t, T: T + c).set_integrator("dopri5", atol=1.0e-3, rtol=1.0e-3)
r.set_initial_value(1.0)

t = [0.0]
T = [1.0]
while r.successful() and r.t < 10.0:
    r.integrate(r.t + 1.0)
    t.append(r.t)
    T.append(r.y[0])

print(T, end='')
