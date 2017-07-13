import os
import unittest
from Implementation import param
from Implementation import inputFormat
 
class TestInputFormat(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_get_input(self):
        params = param.Param()
        inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput2.txt"), params)
        self.assertEqual(2000.0, params.a)
        self.assertEqual(1600.0, params.b)
        self.assertEqual(10.0, params.t)
        self.assertEqual("FT", params.gt)
        self.assertEqual(15, params.w)
        self.assertEqual(1.0, params.tnt)
        self.assertEqual((0.5, 1.2, 9.0), params.sdvect)
        self.assertEqual(0.009, params.pbtol)
        
if __name__ == '__main__':
    unittest.main()
