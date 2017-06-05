import unittest
from . import testInterp
from . import testInterp2
from . import testInterp3
from . import testInterp4
from . import testInterp5
from . import testInterp6
from . import testInterp7
from . import testInterp8
from . import testInterp9
from . import testInterp10
from . import testInterp11
from . import testInterp12
from . import testInterp13
from . import testInterp14

class InterpTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testInterp.TestInterp, testInterp2.TestInterp, testInterp3.TestInterp,
                             testInterp4.TestInterp, testInterp5.TestInterp, testInterp6.TestInterp,
                             testInterp7.TestInterp, testInterp8.TestInterp, testInterp9.TestInterp,
                             testInterp10.TestInterp, testInterp11.TestInterp, testInterp12.TestInterp,
                             testInterp13.TestInterp, testInterp14.TestInterp]))

    testSuite.run(testResult)
    interp_results = testResult.testsRun
    interp_failures = len(testResult.failures)
    interp_successes = interp_results - interp_failures

    print("Ran " + str(interp_results) + " tests for interp.py. " + str(interp_successes) + \
          " passed and " + str(interp_failures) + " failed.")

    if interp_failures > 0:
        print(testResult.failures)
