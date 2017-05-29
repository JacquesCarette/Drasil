import unittest
from . import testDerivedValues
from . import testDerivedValues2
from . import testDerivedValues3

class DerivedValTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()
    
    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testDerivedValues.TestDerivedValues, testDerivedValues2.TestDerivedValues,
                             testDerivedValues3.TestDerivedValues]))

    testSuite.run(testResult)
    derivedVal_results = testResult.testsRun
    derivedVal_failures = len(testResult.failures)
    derivedVal_successes = derivedVal_results -derivedVal_failures

    print("Ran " + str(derivedVal_results) + " tests for derivedValues.py. " + str(derivedVal_successes) + \
          " passed and " + str(derivedVal_failures) + " failed.")
      
    if derivedVal_failures > 0:
        print(testResult.failures)
