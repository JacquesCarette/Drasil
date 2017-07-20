import os
import unittest
from Implementation import checkConstraints
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues


class TestCheckConstraints(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInvalidInput10.txt"), self.params) # sd > 130 
        self.derivedValues = derivedValues.derived_params(self.params)  
    
    def test_check_constraints(self):
        with self.assertRaises(SystemExit) as context:
            checkConstraints.check_constraints(self.params)

        self.assertEqual(("InputError: SD must be between 6 and 130"), context.exception.args[0])
            
        
if __name__ == "__main__":
    unittest.main()
