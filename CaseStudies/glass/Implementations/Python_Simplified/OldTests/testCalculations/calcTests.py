import os
import unittest
from . import testAllCalculations

class CalcTests(unittest.TestSuite):    

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests( 
      map (
        (lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
        [ testAllCalculations.TestCalculations ]
      )
    )

    testSuite.run(testResult)
    calc_results = testResult.testsRun
    calc_failures = len(testResult.failures)
    calc_successes = calc_results - calc_failures

    print("Ran " + str(calc_results) + " tests for calculations.py. " + str(calc_successes) + \
          " passed and " + str(calc_failures) + " failed.")

    if calc_failures > 0:
        print(testResult.failures)
