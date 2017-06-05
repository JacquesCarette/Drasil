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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles","testInput5.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(1.5060545597945577e+01),self.params)
        self.assertEqual(pb,3.170717616254970e-02)
        
    def test_calc_lr(self):
        nfl = calculations.calc_nfl(float64(4.1523490997079925e+01), self.params)
        lr = calculations.calc_lr(nfl, self.params)
        self.assertTupleEqual((lr,nfl),(6.8430021557880369,3.4215010778940185))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(3.170717616254970e-02),\
        float64(6.8430021557880369),float64(9.0124715535095277e+00),self.params)
        self.assertTupleEqual((is_safe1,is_safe2,safe),(False,False,\
        'For the given input parameters, the glass is NOT considered safe'))
     
if __name__ == '__main__':
    unittest.main()
