import unittest
from . import testCheckConstraints
from . import testCheckConstraints2
from . import testCheckConstraints3
from . import testCheckConstraints4
from . import testCheckConstraints5
from . import testCheckConstraints6
from . import testCheckConstraints7
from . import testCheckConstraints8
from . import testCheckConstraints9
from . import testCheckConstraints10
from . import testCheckConstraints11
from . import testCheckConstraints12
from . import testCheckConstraints13
from . import testCheckConstraints14
from . import testCheckConstraints15
from . import testCheckConstraints16
from . import testCheckConstraints17
from . import testCheckConstraints18
from . import testCheckConstraints19

class CheckConTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()  

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testCheckConstraints.TestCheckConstraints, testCheckConstraints2.TestCheckConstraints,
                             testCheckConstraints3.TestCheckConstraints, testCheckConstraints4.TestCheckConstraints,
                             testCheckConstraints5.TestCheckConstraints, testCheckConstraints6.TestCheckConstraints,
                             testCheckConstraints7.TestCheckConstraints, testCheckConstraints8.TestCheckConstraints,
                             testCheckConstraints9.TestCheckConstraints, testCheckConstraints10.TestCheckConstraints,
                             testCheckConstraints11.TestCheckConstraints, testCheckConstraints12.TestCheckConstraints,
                             testCheckConstraints13.TestCheckConstraints, testCheckConstraints14.TestCheckConstraints,
                             testCheckConstraints15.TestCheckConstraints, testCheckConstraints16.TestCheckConstraints,
                             testCheckConstraints17.TestCheckConstraints, testCheckConstraints18.TestCheckConstraints,
                             testCheckConstraints19.TestCheckConstraints]))

    testSuite.run(testResult)
    checkCon_results = testResult.testsRun
    checkCon_failures = len(testResult.failures)
    checkCon_successes = checkCon_results - checkCon_failures

    print("Ran " + str(checkCon_results) + " tests for checkConstraints.py. " + str(checkCon_successes) + \
          " passed and " + str(checkCon_failures) + " failed.")

    if checkCon_failures > 0:
        print(testResult.failures)
