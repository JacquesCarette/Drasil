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

"""
fileName          --> [inputFileName     , val1                  , val2                  , q_hat_tol_value       , lrShouldBe            , nflShouldBe           , lrVal                 , qVal                  , is_safe1ShouldBe, is_safe2ShouldBe]
TestCalculations  --> ["defaultInput.txt", 9.548951781296090e+00 , 1.3015245902037176e-04, 4.152349099707993e+01 , 6.8430021557880387    , 3.4215010778940194    , 6.8430021557880387e+00, 3.2582859920186165e+00, True            , True            ] # move this file to 'Test/InputFiles'
TestCalculations2 --> ["testInput1.txt"  , 1.076507412044263e+01 , 1.8246621494248938e-03, 3.257174731680539e+01 , 4.916016610996773     , 4.916016610996773     , 4.916016610996773     , 3.658003449421614     , True            , True            ]
TestCalculations3 --> ["testInput2.txt"  , 1.225261500913697e+01 , 3.459068155453604e-04 , 5.895281852290154e+01 , 1.0929742089945215e+01, 2.7324355224863037e+00, 1.092974208994522e+01 , 5.777809021268771e+00 , True            , True            ]
TestCalculations4 --> ["testInput3.txt"  , 9.5489517812960898e+00, 1.3015245902037176e-04, 1.8959080873869194e+01, 3.1244249502232408e+00, 1.5622124751116204e+00, 3.1244249502232408e+00, 3.2582859920186165e+00, False           , False           ]
TestCalculations5 --> ["testInput4.txt"  , 1.440913593602348e+01 , 1.665671993597151e-02 , 4.152349099707993e+01 , 6.8430021557880387    , 3.4215010778940194    , 6.8430021557880387    , 7.932494779912231e+00 , False           , False           ]
TestCalculations6 --> ["testInput5.txt"  , 1.5060545597945577e+01, 3.170717616254970e-02 , 4.1523490997079925e+01, 6.8430021557880369    , 3.4215010778940185    , 6.8430021557880369    , 9.0124715535095277e+00, False           , False           ]
TestCalculations7 --> ["testInput6.txt"  , 3.305295130843113e+01 , 2.5284182622823503e-01, 9.486537171669954e+02 , 1.7169821748456926e+00, 8.584910874228463e-01 , 1.7169821748456926e+00, 3.258285992018616e+00 , False           , False           ]
"""