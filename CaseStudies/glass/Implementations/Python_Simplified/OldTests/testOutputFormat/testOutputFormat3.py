import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from Implementation import outputFormat

class TestOutputFormat(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput4.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
        self.output = outputFormat.display_output("testoutput.txt", 7.377747177423622e+00, 1.406670550988096e+01,
                                                4.152349099707993e+01, 1.185574651484522e-02, 6.843002155788037e+00, 
                                                3.421501077894018e+00, 0.000000, 0.000000,
                                                "For the given input parameters, the glass is NOT considered safe", self.params)
   
    def test_display_output(self):
        f1 = open(os.path.join("Test/Inputfiles", "output4.txt"), "r")
        f2 = open("testoutput.txt", "r")
        text1 = f1.readlines()
        text2 = f2.readlines()
        self.assertEqual(text1, text2)
        f1.close()
        f2.close()
        
if __name__ == "__main__":
        unittest.main()
