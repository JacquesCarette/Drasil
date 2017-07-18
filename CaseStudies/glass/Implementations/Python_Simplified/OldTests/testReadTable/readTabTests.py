import unittest
from . import testReadTable
from . import testReadTable2

class ReadTabTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testReadTable.TestReadTable, testReadTable2.TestReadTable]))
                            
    testSuite.run(testResult)
    readTab_results = testResult.testsRun
    readTab_failures = len(testResult.failures)
    readTab_successes = readTab_results - readTab_failures

    print("Ran " + str(readTab_results) + " tests for readTable.py. " + str(readTab_successes) + \
          " passed and " + str(readTab_failures) + " failed.")

    if readTab_failures > 0:
        print(testResult.failures)
