import os
import unittest
from Implementation import checkConstraints
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues

class TestCheckConstraints(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles","testInvalidInput5.txt"),self.params) # t not in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]
        self.derivedValues = derivedValues.derived_params(self.params)
    
    def test_check_constraints(self):
        with self.assertRaises(SystemExit) as context:
            checkConstraints.check_constraints(self.params)

        self.assertEqual(("InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]"), context.exception.code)
        

if __name__ == "__main__":
    unittest.main()
