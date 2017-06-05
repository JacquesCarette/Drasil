import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from Implementation import outputFormat

class TestOutputFormat(unittest.TestCase):
    
    def setUp(self):
        self.params=param.Param()
        self.inputFormat=inputFormat.get_input(os.path.join("Test/Inputfiles","testInput3.txt"), self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
        self.output=outputFormat.display_output("testoutput.txt",3.258285992018616e+00,9.548951781296090e+00,
                                                1.895908087386919e+01,1.301524590203718e-04,3.124424950223241e+00, 
                                                1.562212475111620e+00,0.000000,0.000000, "For the given input parameters, the glass is NOT considered safe", self.params)
         
    def test_display_output(self):
        f1=open(os.path.join("Test/Inputfiles","output3.txt"),"r")
        f2=open("testoutput.txt","r")
        text1=f1.readlines()
        text2=f2.readlines()
        self.assertEqual(text1,text2)
        f1.close()
        f2.close()
        
if __name__ == "__main__":
        unittest.main()
