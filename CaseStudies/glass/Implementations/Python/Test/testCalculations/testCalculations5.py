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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput4.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(1.440913593602348e+01), self.params)
        self.assertEqual(pb, 1.665671993597151e-02)
        
    def test_calc_lr(self):
        lr, nfl = calculations.calc_lr(float64(4.152349099707993e+01), self.params)
        self.assertTupleEqual((lr, nfl), (6.8430021557880387, 3.4215010778940194))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(1.665671993597151e-02),\
        float64(6.8430021557880387), float64(7.932494779912231e+00), self.params)
        self.assertTupleEqual((is_safe1, is_safe2, safe), (False, False,\
        'For the given input parameters, the glass is NOT considered safe'))
     
if __name__ == '__main__':
    unittest.main()
