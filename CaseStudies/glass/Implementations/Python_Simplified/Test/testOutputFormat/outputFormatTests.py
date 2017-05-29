import unittest
from . import testOutputFormat
from . import testOutputFormat2
from . import testOutputFormat3

class OutputFormatTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testOutputFormat.TestOutputFormat, testOutputFormat2.TestOutputFormat,
                             testOutputFormat3.TestOutputFormat]))

    testSuite.run(testResult)
    outputFormat_results = testResult.testsRun
    outputFormat_failures = len(testResult.failures)
    outputFormat_successes = outputFormat_results - outputFormat_failures
    
    print("Ran " + str(outputFormat_results) + " tests for outputFormat.py. " + str(outputFormat_successes) + \
          " passed and " + str(outputFormat_failures) + " failed.")

    if outputFormat_failures > 0:
        print(testResult.failures)
