'''
Test when the input file for mainfun.py is "testInput2.txt"
'''
import os
import unittest
from Implementation import mainfun

class TestMainfun(unittest.TestCase):
    
    def setUp(self):
        self.output = mainfun.main(os.path.join("Test/Inputfiles", "testInput2.txt"))
        
    def test_main(self):
        f1 = open("outputfile.txt", 'r')
        f2 = open(os.path.join("Test/Inputfiles", "output2.txt"), 'r')
        text1 = f1.readlines()
        text2 = f2.readlines()
        self.assertEqual(text1, text2)
        f1.close()
        f2.close()

if __name__ == '__main__':
    unittest.main()
