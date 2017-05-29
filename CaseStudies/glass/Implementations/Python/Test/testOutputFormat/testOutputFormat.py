import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from Implementation import outputFormat

class TestOutputFormat(unittest.TestCase):
    
    def setUp(self):
        self.params=param.Param()
        self.inputFormat=inputFormat.get_input("defaultInput.txt", self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
        self.output=outputFormat.display_output("testoutput.txt",3.258285992018616e+00,9.548951781296090e+00,
                                                4.152349099707993e+01,1.301524590203718e-04,6.843002155788037e+00, 
                                                3.421501077894018e+00,1.000000,1.000000, "For the given input parameters, the glass is considered safe", self.params)
         
    def test_display_output(self):
        f1=open(os.path.join("Test/Inputfiles","output.txt"),"r")
        f2=open("testoutput.txt","r")
        text1=f1.readlines()
        text2=f2.readlines()
        self.assertEqual(text1,text2)
        f1.close()
        f2.close()
        
if __name__ == "__main__":
        unittest.main()
