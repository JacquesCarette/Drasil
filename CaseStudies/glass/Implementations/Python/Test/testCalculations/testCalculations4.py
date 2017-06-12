import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from numpy import float64
from Implementation import calculations
 
class TestCalculations(unittest.TestCase):
 
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput3.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(9.5489517812960898e+00), self.params)
        self.assertEqual(pb, 1.3015245902037176e-04)
        
    def test_calc_lr(self):
        lr, nfl = calculations.calc_lr(float64(1.8959080873869194e+01), self.params)
        self.assertTupleEqual((lr, nfl), (3.1244249502232408e+00, 1.5622124751116204e+00))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(1.3015245902037176e-04),\
        float64(3.1244249502232408e+00), float64(3.2582859920186165e+00), self.params)
        self.assertTupleEqual((is_safe1, is_safe2, safe), (False, False,\
        'For the given input parameters, the glass is NOT considered safe'))
     
if __name__ == '__main__':
    unittest.main()
