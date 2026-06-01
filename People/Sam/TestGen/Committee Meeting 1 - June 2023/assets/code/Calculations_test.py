## \file Calculations_test.py
#  \author Samuel J. Crawford
#  \brief Runs tests for the calculations of values

from math import isclose
from pytest import mark, raises
from unittest.mock import Mock

from python import Calculations
from .TestHelpers import get_expected

# A:gravAccelValue states "The acceleration due to gravity is assumed to have
# the value [9.8]", so any other value for g is invalid
valid_g = 9.8
invalid_g_values = [-valid_g, 0, valid_g * 2]

epsilon = 0.02

def build_mocks(*attrs):
    mocks = []
    defaults = ["v_launch", "theta", "p_target"]
    for d in get_expected(*(defaults + list(attrs))):
        mock_attrs = dict()
        for i, attr in enumerate(defaults + list(attrs), start=1):
            mock_attrs[attr] = d[i]
        mock = Mock()
        mock.configure_mock(**mock_attrs)
        mocks.append(mock)
    return mocks

## \brief Tests calculation of t_flight with valid input
@mark.parametrize("mock", build_mocks("t_flight"))
def test_func_t_flight_valid(mock):
    assert isclose(Calculations.func_t_flight(mock, valid_g), mock.t_flight)

## \brief Tests calculation of p_land with valid input
@mark.parametrize("mock", build_mocks("p_land"))
def test_func_p_land_valid(mock):
    assert isclose(Calculations.func_p_land(mock, valid_g), mock.p_land)

## \brief Tests calculation of d_offset with valid input
@mark.parametrize("mock", build_mocks("p_land", "d_offset"))
def test_func_d_offset_valid(mock):
    assert isclose(Calculations.func_d_offset(mock, mock.p_land), mock.d_offset)
    
## \brief Tests calculation of s with valid input
@mark.parametrize("mock", build_mocks("d_offset", "s"))
def test_func_s_valid(mock):
    assert Calculations.func_s(mock, epsilon, mock.d_offset) == mock.s

# TODO: should the following tests also be made into Control tests?
## \brief Tests calculation of t_flight with invalid gravitational \n
#  acceleration and valid input
@mark.parametrize("mock", build_mocks())
@mark.parametrize("invalid_g", invalid_g_values)
@mark.xfail
def test_func_t_flight_invalid_g(mock, invalid_g):
    with raises(AssertionError):
        Calculations.func_t_flight(mock, invalid_g)

## \brief Tests calculation of p_land with invalid gravitational \n
#  acceleration and valid input
@mark.parametrize("mock", build_mocks())
@mark.parametrize("invalid_g", invalid_g_values)
@mark.xfail
def test_func_p_land_invalid_g(mock, invalid_g):
    with raises(AssertionError):
        Calculations.func_p_land(mock, invalid_g)
