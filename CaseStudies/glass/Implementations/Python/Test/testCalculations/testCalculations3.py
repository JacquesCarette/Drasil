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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput2.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(1.225261500913697e+01), self.params)
        self.assertEqual(pb, 3.459068155453604e-04)
    
    def test_calc_lr(self):
        lr, nfl = calculations.calc_lr(float64(5.895281852290154e+01), self.params)
        self.assertTupleEqual((lr, nfl), (1.0929742089945215e+01, 2.7324355224863037e+00))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(3.459068155453604e-04),\
        float64(1.092974208994522e+01), float64(5.777809021268771e+00), self.params)
        self.assertTupleEqual((is_safe1, is_safe2, safe), (True, True,\
        'For the given input parameters, the glass is considered safe'))
        
if __name__ == '__main__':
    unittest.main()
