"""
Input Constraints Module
Secret: The constraints on the input data.
Service: Defines the constraints on the input data and gives and error if a constraint is violated.
"""


def check_constraints(params):
    if params.a <= 0 or params.b <= 0:
        raise SystemExit("InputError: a and b must be greater than 0")
    if params.asprat < 1 or params.asprat > 5:
        raise SystemExit("InputError: a/b must be between 1 and 5")
    if not (params.t in ["2.50","2.70","3.00","4.00","5.00","6.00","8.00","10.00","12.00","16.00","19.00","22.00"]):
        raise SystemExit("InputError: t must be in [2.50,2.70,3.00,4.00,5.00,6.00,8.00,10.00,12.00,16.00,19.00,22.00]")
    if params.tnt <= 0:
        raise SystemExit("InputError: TNT must be greater than 0")
    if params.wtnt < 4.5 or params.wtnt > 910:
        raise SystemExit("InputError: wtnt must be between 4.5 and 910")
    if params.sd<6 or params.sd>130:
        raise SystemExit("InputError: SD must be between 6 and 130")
