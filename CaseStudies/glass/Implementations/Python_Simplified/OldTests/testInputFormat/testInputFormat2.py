import os
import unittest
from Implementation import param
from Implementation import inputFormat
 
class TestInputFormat(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_get_input(self):
        params = param.Param()
        inputFormat.get_input(os.path.join("Test/Inputfiles", "testInput1.txt"), params)
        self.assertEqual(1200.0, params.a)
        self.assertEqual(1000.0, params.b)
        self.assertEqual(8.0, params.t)
        self.assertEqual("AN", params.gt)
        self.assertEqual(10, params.w)
        self.assertEqual(1.0, params.tnt)
        self.assertEqual((0.0, 2.0, 10.0), params.sdvect)
        self.assertEqual(0.010, params.pbtol)
        
if __name__ == '__main__':
    unittest.main()
