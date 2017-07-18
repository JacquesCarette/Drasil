import os
import unittest
from Implementation import checkConstraints
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues


class TestCheckConstraints(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput10.txt"), self.params) # wtnt = 910 
        self.derivedValues = derivedValues.derived_params(self.params)  
    
    def test_check_constraints(self):
        try:
            checkConstraints.check_constraints(self.params)
        except:
            self.fail("Encountered an unexpected exception")
            
        
if __name__ == "__main__":
    unittest.main()
