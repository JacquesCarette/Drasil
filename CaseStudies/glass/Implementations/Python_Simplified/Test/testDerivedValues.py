import os
import unittest
from Implementation import param
from Implementation import derivedValues
from Implementation import inputFormat

class TestDerivedValues(unittest.TestCase):
    
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "derivedValues.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests = len(input)
        self.inputFileName    = [row[0]               for row in input]
        self.arExpected       = [float(row[1])        for row in input]
        self.sdExpected       = [float(row[2])        for row in input]
        self.ldfExpected      = [float(row[3])        for row in input]
        self.wtntExpected     = [float(row[4])        for row in input]
        self.hExpected        = [float(row[5])        for row in input]
        self.gtfExpected      = [int(row[6].rstrip()) for row in input]
        self.params           = [param.Param()        for i   in range(self.numTests)]
        for i in range(self.numTests):
            inputFormat.get_input(os.path.join("Test/Inputfiles", self.inputFileName[i]), self.params[i])
            derivedValues.derived_params(self.params[i])
    
    def test_derived_params(self):
        for i in range(self.numTests): #AttributeError: 'TestDerivedValues' object has no attribute 'numTests'
            with self.subTest(i=i):
                self.assertEqual(self.arExpected[i], self.params[i].asprat)
                self.assertEqual(self.sdExpected[i], self.params[i].sd)
                self.assertEqual(self.ldfExpected[i], self.params[i].ldf)
                self.assertEqual(self.wtntExpected[i], self.params[i].wtnt)
                self.assertEqual(self.hExpected[i], self.params[i].h)
                self.assertEqual(self.gtfExpected[i], self.params[i].gtf)
        
if __name__ == '__main__':
    unittest.main()