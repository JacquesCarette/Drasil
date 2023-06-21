## \file Calculations_test.py
#  \author Samuel J. Crawford
#  \brief Runs tests for the output of calculated values

from pytest import mark

from python import OutputFormat
from .TestHelpers import get_expected

## \brief Tests writing valid input
@mark.parametrize("s, d_offset, t_flight", get_expected("d_offset", "t_flight"))
def test_get_input_valid(s, d_offset, t_flight):
    OutputFormat.write_output(s, d_offset, t_flight)
    with open("output.txt") as f:
        assert f.readlines() == [f"s = {s}\n",
                                 f"d_offset = {d_offset}\n",
                                 f"t_flight = {t_flight}\n",]
