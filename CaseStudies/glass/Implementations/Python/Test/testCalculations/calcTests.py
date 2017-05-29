import unittest
from . import testCalculations
from . import testCalculations2
from . import testCalculations3
from . import testCalculations4
from . import testCalculations5
from . import testCalculations6
from . import testCalculations7

class CalcTests(unittest.TestSuite):    

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testCalculations.TestCalculations, testCalculations2.TestCalculations,
                             testCalculations3.TestCalculations, testCalculations4.TestCalculations,
                             testCalculations5.TestCalculations, testCalculations6.TestCalculations,
                             testCalculations7.TestCalculations]))

    testSuite.run(testResult)
    calc_results = testResult.testsRun
    calc_failures = len(testResult.failures)
    calc_successes = calc_results - calc_failures

    print("Ran " + str(calc_results) + " tests for calculations.py. " + str(calc_successes) + \
          " passed and " + str(calc_failures) + " failed.")

    if calc_failures > 0:
        print(testResult.failures)
