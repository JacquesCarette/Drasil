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

#####

class TestCheckConstraints(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", inputFileName), self.params) # a < 0
        self.derivedValues = derivedValues.derived_params(self.params)
    
    def test_check_constraints(self):
        with self.assertRaises(SystemExit) as context:
            checkConstraints.check_constraints(self.params)

        self.assertEqual((errorMsg), context.exception.args[0])
    

if __name__ == "__main__":
    unittest.main()

#####


"""
constraints1.txt
fileName                 , inputFileName           , errorMsg
testCheckConstraints.py  , "testInvalidInput1.txt" , "InputError: a and b must be greater than 0"
testCheckConstraints2.py , "testInvalidInput2.txt" , "InputError: a and b must be greater than 0"
testCheckConstraints3.py , "testInvalidInput3.txt" , "InputError: a/b must be between 1 and 5"
testCheckConstraints4.py , "testInvalidInput4.txt" , "InputError: a/b must be between 1 and 5"
testCheckConstraints5.py , "testInvalidInput5.txt" , "InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]"
testCheckConstraints6.py , "testInvalidInput6.txt" , "InputError: wtnt must be between 4.5 and 910"
testCheckConstraints7.py , "testInvalidInput7.txt" , "InputError: wtnt must be between 4.5 and 910"
testCheckConstraints8.py , "testInvalidInput8.txt" , "InputError: TNT must be greater than 0"
testCheckConstraints9.py , "testInvalidInput9.txt" , "InputError: SD must be between 6 and 130"
testCheckConstraints10.py, "testInvalidInput10.txt", "InputError: SD must be between 6 and 130"
testCheckConstraints11.py, "testInvalidInput11.txt", "InputError: a and b must be greater than 0"
testCheckConstraints12.py, "testInvalidInput12.txt", "InputError: a and b must be greater than 0"
testCheckConstraints13.py, "testInvalidInput13.txt", "InputError: TNT must be greater than 0"
"""

"""
    def test_check_constraints(self):
        try:
            checkConstraints.check_constraints(self.params)
        except:
            self.fail("Encountered an unexpected exception")
"""

"""
constraints2.txt
fileName                 , inputFileName
testCheckConstraints14.py, "testInput7.txt"
testCheckConstraints15.py, "testInput8.txt"
testCheckConstraints16.py, "testInput9.txt"
testCheckConstraints17.py, "testInput10.txt"
testCheckConstraints18.py, "testInput11.txt"
testCheckConstraints19.py, "testInput12.txt"
"""
