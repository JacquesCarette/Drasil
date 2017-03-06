import unittest
from Implementation import param
from Implementation import inputFormat
 
class TestInputFormat(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_get_input(self):
        params = param.Param()
        inputFormat.get_input("defaultInput.txt", params)
        self.assertEqual(1600.0, params.a)
        self.assertEqual(1500.0, params.b)
        self.assertEqual(10.0, params.t)
        self.assertEqual("HS", params.gt)
        self.assertEqual(10, params.w)
        self.assertEqual(1.0, params.tnt)
        self.assertEqual((0.0, 1.5, 11.0), params.sdvect)
        self.assertEqual(0.008, params.pbtol)
        
if __name__ == '__main__':
    unittest.main()
