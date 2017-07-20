import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from Implementation import calculations
 
class TestCalculations(unittest.TestCase):
 
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "calculations.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests = len(input)
        self.inputFileName    = [row[0] for row in input]
        self.val1             = [float(row[1]) for row in input]
        self.val2             = [float(row[2]) for row in input]
        self.q_hat_tol_val    = [float(row[3]) for row in input]
        self.lrShouldBe       = [float(row[4]) for row in input]
        self.nflShouldBe      = [float(row[5]) for row in input]
        self.lrVal            = [float(row[6]) for row in input]
        self.qVal             = [float(row[7]) for row in input]
        self.is_safe1ShouldBe = [row[8].rstrip() == "True" for row in input]
        self.is_safe2ShouldBe = [row[9].rstrip() == "True" for row in input]
        self.params           = [param.Param() for i in range(self.numTests)]
        for i in range(self.numTests):
            inputFormat.get_input(os.path.join("Test/Inputfiles", self.inputFileName[i]), self.params[i])
            derivedValues.derived_params(self.params[i])

    def test_calc_pb(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                pb = calculations.calc_pb(self.val1[i], self.params[i])
                self.assertAlmostEqual(pb, self.val2[i])
               
    def test_calc_lr(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                nfl = calculations.calc_nfl(self.q_hat_tol_val[i], self.params[i])
                lr = calculations.calc_lr(nfl, self.params[i])
                self.assertTupleEqual((lr, nfl), (self.lrShouldBe[i], self.nflShouldBe[i]))
        
    def test_is_safe(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                is_safe1 = calculations.is_safe1(self.val2[i], self.params[i])
                is_safe2 = calculations.is_safe2(self.lrVal[i], self.qVal[i])
                self.assertTupleEqual((is_safe1, is_safe2), (self.is_safe1ShouldBe[i], self.is_safe2ShouldBe[i]))
                
        
if __name__ == '__main__':
    unittest.main()