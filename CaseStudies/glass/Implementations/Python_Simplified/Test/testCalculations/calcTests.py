import os
import unittest
from . import testAllCalculations

class CalcTests(unittest.TestSuite):    

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    """
    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testAllCalculations.TestCalculations(inputFileName, val1,
                              val2, q_hat_tol_val, lrShouldBe, nflShouldBe, lrVal, qVal,
                              is_safe1ShouldBe, is_safe2ShouldBe), testCalculations.TestCalculations, testCalculations2.TestCalculations,
                             testCalculations3.TestCalculations, testCalculations4.TestCalculations,
                             testCalculations5.TestCalculations, testCalculations6.TestCalculations,
                             testCalculations7.TestCalculations]))
    """

    infile = open ("calculations.txt", "r")
    
    for i in range(7):
      text = infile.readline().rstrip()
      text.split(",")
      
      #Get values for test from line
      inputFileName    = text[0]
      val1             = float(text[1])
      val2             = float(text[2])
      q_hat_tol_val    = float(text[3])
      lrShouldBe       = float(text[4])
      nflShouldBe      = float(text[5])
      lrVal            = float(text[6])
      qVal             = float(text[7])
      is_safe1ShouldBe = text[8]
      is_safe2ShouldBe = text[9]

      testSuite.addTests((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                                  [testAllCalculations.TestCalculations(inputFileName, val1,
                                  val2, q_hat_tol_val, lrShouldBe, nflShouldBe, lrVal, qVal,
                                  is_safe1ShouldBe, is_safe2ShouldBe)])

    testSuite.run(testResult)
    calc_results = testResult.testsRun
    calc_failures = len(testResult.failures)
    calc_successes = calc_results - calc_failures

    print("Ran " + str(calc_results) + " tests for calculations.py. " + str(calc_successes) + \
          " passed and " + str(calc_failures) + " failed.")

    if calc_failures > 0:
        print(testResult.failures)
