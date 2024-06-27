# Terminology and Definitions {#Sec:TermDefs}

This subsection provides a list of terms that are used in the subsequent sections and their meaning, with the purpose of reducing ambiguity and making it easier to correctly understand the requirements.

- PD Control Loop: Closed-Loop control system with PD Controller, Summing Point and Power Plant.
- PD Controller: Proportional-Derivative Controller.
- Summing Point: Control block where the difference between the Set-Point and the Process Variable is computed.
- Power Plant: A second order system to be controlled.
- Second Order System: A system whose input-output relationship is denoted by a second-order differential equation.
- Process Error: Input to the PID controller. Process Error is the difference between the Set-Point and the Process Variable.
- Simulation Time: Total execution time of the PD simulation.
- Process Variable: The output value from the power plant.
- Set-Point: The desired value that the control system must reach. This also knows as the reference variable.
- Proportional Gain: Gain constant of the proportional controller.
- Derivative Gain: Gain constant of the derivative controller.
- Proportional control: A linear feedback control system where correction is applied to the controlled variable which is proportional to the difference between desired and measured values.
- Derivative control: Monitors the rate of change of the error signal and contributes a component of the output signal (proportional to a derivative of the error signal).
- Frequency domain: The analysis of mathematical functions in terms of frequency, instead of time.
- Time domain: The analysis of mathematical functions in terms of time.
- Laplace transform: An integral transform that converts a function of a real variable t (often time) to a function of a complex variable s (complex frequency).
- Control Variable: The Control Variable is the output of the PD controller.
- Step Time: Simulation step time.
- Absolute Tolerance: Absolute tolerance for the integrator.
- Relative Tolerance: Relative tolerance for the integrator.
- Transfer Function: The Transfer Function of a system is the ratio of the output to the input functions in the frequency domain.
- Damping Coefficient: Quantity that characterizes a second order system's oscillatory response.
- Stiffness Coefficient: Quantity that characterizes a spring's stiffness.

