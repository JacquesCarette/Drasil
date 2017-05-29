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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles","testInput1.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(1.076507412044263e+01), self.params)
        self.assertEqual(pb, 1.8246621494248938e-03)
        
    def test_calc_lr(self):
        nfl = calculations.calc_nfl(float64(3.257174731680539e+01), self.params)
        lr = calculations.calc_lr(nfl, self.params)
        self.assertTupleEqual((lr,nfl),(4.916016610996773,4.916016610996773))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(1.8246621494248938e-03),\
        float64(4.916016610996773),float64(3.658003449421614),self.params)
        self.assertTupleEqual((is_safe1,is_safe2,safe),(True,True,\
        'For the given input parameters, the glass is considered safe'))
        
if __name__ == '__main__':
    unittest.main()
