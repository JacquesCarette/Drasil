"""
Output Format Module
Secret: The format and structure of the output data.
Service: Outputs the results of the calculations, including the input parameters, the demand, the capacity,
the probability of breakage, and both safety requirements.
"""
def display_output(filename, q, j, q_hat_tol, pb, lr, nfl, is_safe1, is_safe2, params):

    f = open(filename, 'w')
    for attr, value in sorted(params.__dict__.items()):
        string_param = attr+"\t"+str(value)+"\n"
        f.write(string_param)

    f.write("Demand (q) %.15e\n" % q)
    f.write("Stress Distribution Factor (J) %.15e\n" % j)
    f.write("Tolerable Pressure %.15e\n" % q_hat_tol)
    f.write("Probability of Breakage (Pb) %.15e\n" % pb)
    f.write("Capacity (LR)%.15e\n" % lr)
    f.write("Non-Factored Load (NFL)%.15e\n" % nfl)
    f.write("Safety Requirement-1 %f\n" % is_safe1)
    f.write("Safety Requirement-2 %f\n" % is_safe2)
    if is_safe1 and is_safe2:
        f.write("For the given input parameters, the glass is considered safe\n")
    else:
        f.write("For the given input parameters, the glass is NOT considered safe\n")  
    f.close()
