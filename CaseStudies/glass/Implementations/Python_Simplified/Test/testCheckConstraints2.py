import os
import unittest
from Implementation import checkConstraints
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues


class TestCheckConstraints(unittest.TestCase):
    
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "constraints2.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests      = len(input)
        self.inputFileName = [row[0].rstrip() for row in input]
        self.params        = [param.Param()   for i in range(self.numTests)]

        for i in range (self.numTests):
            inputFormat.get_input(os.path.join("Test/Inputfiles", self.inputFileName[i]), self.params[i])
            derivedValues.derived_params(self.params[i])
    
    def test_check_constraints(self):
        try:
            checkConstraints.check_constraints(self.params[i])
        except:
            self.fail("Encountered an unexpected exception")

if __name__ == "__main__":
    unittest.main()
