import os
import unittest
from Implementation import param
from Implementation import derivedValues
from Implementation import inputFormat

class TestDerivedValues(unittest.TestCase):
    
    def setup(self):
        pass
    
    def test_derived_params(self):
        params = param.Param()
        inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput2.txt"), params)
        derivedValues.derived_params(params)
        self.assertEqual(1.25, params.asprat)
        self.assertEqual(9.093404203047394, params.sd)
        self.assertEqual(0.2696493494752911, params.ldf)
        self.assertEqual(15.0, params.wtnt)
        self.assertEqual(9.02, params.h)
        self.assertEqual(4, params.gtf)
        
if __name__ == '__main__':
    unittest.main()
