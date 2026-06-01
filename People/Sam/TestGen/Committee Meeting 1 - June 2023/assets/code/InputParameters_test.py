## \file InputParameters_test.py
#  \author Samuel J. Crawford
#  \brief Runs tests for the input of parameters

from math import isclose
from pytest import mark, raises

# for capturing stdout
from contextlib import redirect_stdout
from io import StringIO

from . import conftest
from .TestHelpers import get_expected, read_inParams
from .test_input.expected_outputs import invalid_format_input_files, \
    invalid_value_input_files

## \brief Tests reading valid input
@mark.parametrize("filename,v_launch,theta,p_target",
                  get_expected("v_launch", "theta", "p_target"))
def test_get_input_valid(filename, v_launch, theta, p_target):
    assert isclose(conftest.inParams[filename].v_launch, v_launch)
    assert isclose(conftest.inParams[filename].theta, theta)
    assert isclose(conftest.inParams[filename].p_target, p_target)

## \brief Tests reading improperly formatted input
@mark.parametrize("filename", invalid_format_input_files)
def test_get_input_invalid_format(filename):
    # TODO: is this desired behaviour?
    with raises(ValueError):
        read_inParams(filename)

## \brief Tests constraint checking valid input
@mark.parametrize("filename", get_expected())
def test_input_constraints_valid(filename):
    stdout = StringIO()
    with redirect_stdout(stdout):  
        conftest.inParams[filename].input_constraints()
    assert stdout.getvalue() == ""

## \brief Tests constraint checking invalid input
@mark.parametrize("filename", invalid_value_input_files)
def test_input_constraints_invalid(filename):
    stdout = StringIO()
    with redirect_stdout(stdout):  
        conftest.inParams[filename].input_constraints()
    assert "Warning: " in stdout.getvalue()
