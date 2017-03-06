import unittest
from Implementation import param
from Implementation import derivedValues
from Implementation import inputFormat

class TestDerivedValues(unittest.TestCase):
    
    def setup(self):
        pass
    
    def test_derived_params(self):
        params=param.Param()
        inputFormat.get_input("defaultInput.txt",params)
        derivedValues.derived_params(params)
        self.assertEqual(1.0666666666666667,params.asprat)
        self.assertEqual(11.10180165558726,params.sd)
        self.assertEqual(0.2696493494752911,params.ldf)
        self.assertEqual(10.0,params.wtnt)
        self.assertEqual(9.02,params.h)
        self.assertEqual(2,params.gtf)
        
if __name__ == '__main__':
    unittest.main()