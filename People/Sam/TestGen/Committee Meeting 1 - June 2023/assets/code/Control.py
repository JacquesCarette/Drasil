## \file Control.py
# \author Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
# \brief Controls the flow of the program
import sys
import argparse

from . import Calculations
from . import InputParameters
from . import OutputFormat

def main():	
	# from https://stackoverflow.com/questions/54071312/how-to-pass-command-line-argument-from-pytest-to-code
	parser = argparse.ArgumentParser(description='Control')
	parser.add_argument('file')
	args = parser.parse_args()
	filename = f"{args.file}"

	# filename = sys.argv[1]

	g = 9.8
	epsilon = 2.0e-2
	inParams = InputParameters.InputParameters(filename)
	t_flight = Calculations.func_t_flight(inParams, g)
	p_land = Calculations.func_p_land(inParams, g)
	d_offset = Calculations.func_d_offset(inParams, p_land)
	s = Calculations.func_s(inParams, epsilon, d_offset)
	OutputFormat.write_output(s, d_offset, t_flight)

if __name__ == "__main__":
	main()
