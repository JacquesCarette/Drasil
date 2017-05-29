import unittest
from . import testMainfun
from . import testMainfun2
from . import testMainfun3
from . import testMainfun4
from . import testMainfun5
from . import testMainfun6
from . import testMainfun7

class MainTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testMainfun.TestMainfun, testMainfun2.TestMainfun,
                             testMainfun3.TestMainfun, testMainfun4.TestMainfun,
                             testMainfun5.TestMainfun, testMainfun6.TestMainfun,
                             testMainfun7.TestMainfun]))

    testSuite.run(testResult)
    main_results = testResult.testsRun
    main_failures = len(testResult.failures)
    main_successes = main_results - main_failures

    print("Ran " + str(main_results) + " tests for mainfun.py. " + str(main_successes) + \
          " passed and " + str(main_failures) + " failed.")

    if main_failures > 0:
        print(testResult.failures)
