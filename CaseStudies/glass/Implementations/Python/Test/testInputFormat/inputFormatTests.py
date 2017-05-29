import unittest
from . import testInputFormat
from . import testInputFormat2
from . import testInputFormat3

class InputFormatTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testInputFormat.TestInputFormat, testInputFormat2.TestInputFormat,
                             testInputFormat3.TestInputFormat]))

    testSuite.run(testResult)
    inputFormat_results = testResult.testsRun
    inputFormat_failures = len(testResult.failures)
    inputFormat_successes = inputFormat_results - inputFormat_failures

    print("Ran " + str(inputFormat_results) + " tests for inputFormat.py. " + str(inputFormat_successes) + \
          " passed and " + str(inputFormat_failures) + " failed.")

    if inputFormat_failures > 0:
        print(testResult.failures)
