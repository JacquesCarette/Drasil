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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles","testInput6.txt"),self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
        
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(3.305295130843113e+01),self.params)
        self.assertEqual(pb,2.5284182622823503e-01)
        
    def test_calc_lr(self):
        nfl = calculations.calc_nfl(float64(9.486537171669954e+02), self.params)
        lr = calculations.calc_lr(nfl, self.params)
        self.assertTupleEqual((lr,nfl),(1.7169821748456926e+00,8.584910874228463e-01))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(2.5284182622823503e-01),\
        float64(1.7169821748456926e+00),float64(3.258285992018616e+00),self.params)
        self.assertTupleEqual((is_safe1,is_safe2,safe),(False,False,\
        'For the given input parameters, the glass is NOT considered safe'))
     
if __name__ == '__main__':
    unittest.main()
