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
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", inputFileName), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
 
    def test_calc_pb(self):
        """
        Test to make sure returns expected value of pb.  Test should
        actually use some epsilon error, instead of equality of floats
        """
        pb = calculations.calc_pb(float64(val1), self.params)
        self.assertEqual(pb, val2)
        
    def test_calc_lr(self):
        nfl = calculations.calc_nfl(float64(q_hat_tol_value), self.params)
        lr = calculations.calc_lr(nfl, self.params)
        self.assertTupleEqual((lr, nfl), (lrShouldBe, nflShouldBe))
        
    def test_is_safe(self):
        is_safe1 = calculations.is_safe1(float64(val2), self.params)
        is_safe2 = calculations.is_safe2(float64(lrVal), float64(qVal))
        self.assertTupleEqual((is_safe1, is_safe2), (is_safe1ShouldBe, is_safe2ShouldBe))
        
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