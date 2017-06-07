import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from numpy import float64
from Implementation import calculations
 
class TestCalculations(unittest.TestCase):
 
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input("defaultInput.txt", self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
    
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(9.548951781296090e+00), self.params)
        self.assertEqual(pb, 1.3015245902037176e-04)
        
    def test_calc_lr(self):
        lr, nfl = calculations.calc_lr(float64(4.152349099707993e+01), self.params)
        self.assertTupleEqual((lr, nfl), (6.8430021557880387, 3.4215010778940194))
        
    def test_is_safe(self):
        is_safe1, is_safe2, safe = calculations.is_safe(float64(1.301524590203718e-04),\
        float64(6.8430021557880387e+00), float64(3.2582859920186165e+00), self.params)
        self.assertTupleEqual((is_safe1, is_safe2, safe), (True, True,\
        'For the given input parameters, the glass is considered safe'))
     
if __name__ == '__main__':
    unittest.main()
