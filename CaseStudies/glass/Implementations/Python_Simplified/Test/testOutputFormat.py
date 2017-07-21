import os
import unittest
from Implementation import param
from Implementation import inputFormat
from Implementation import derivedValues
from Implementation import outputFormat

class TestOutputFormat(unittest.TestCase):
    
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "outputFormat.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests = len(input)
        self.inputFileName  = [row[0] for row in input]
        self.qVal           = [float(row[1])   for row in input]
        self.jVal           = [float(row[2])   for row in input]
        self.qHatTolVal     = [float(row[3])   for row in input]
        self.pbVal          = [float(row[4])   for row in input]
        self.lrVal          = [float(row[5])   for row in input]
        self.nflVal         = [float(row[6])   for row in input]
        self.is_safe1State  = [float(row[7])   for row in input]
        self.is_safe2State  = [float(row[8])   for row in input]
        self.outputFileName = [row[9].rstrip() for row in input]
        
        self.params         = [param.Param() for i in range(self.numTests)]

        for i in range(self.numTests):
            inputFormat.get_input(self.inputFileName[i], self.params[i])
            derivedValues.derived_params(self.params[i])
            outputFormat.display_output("testoutput.txt", self.qVal[i], self.jVal[i],
                                                self.qHatTolVal[i], self.pbVal[i], 
                                                self.lrVal[i], self.nflVal[i],
                                                self.is_safe1State[i], self.is_safe2State[i],
                                                self.params[i]) 
         
    def test_display_output(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                f1 = open(os.path.join("Test/Inputfiles", self.outputFileName[i]), "r")
                f2 = open("testoutput.txt", "r")
                text1 = f1.readlines()
                text2 = f2.readlines()
                self.assertEqual(text1,text2)
                f1.close()
                f2.close()
                
if __name__ == "__main__":
        unittest.main()