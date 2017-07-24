import os
import unittest
from Implementation import mainfun

class TestMainfun(unittest.TestCase):
    
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "main.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests = len(input)
        self.inputFileName    = [row[0] for row in input]
        self.outputFileName   = [row[1].rstrip() for row in input]
        for i in range(self.numTests):
            mainfun.main(os.path.join("Test/Inputfiles", self.inputFileName[i]))
        
    def test_main(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                f1 = open("outputfile.txt", 'r')
                f2 = open(os.path.join("Test/Inputfiles", self.outputFileName[i]), 'r')
                text1 = f1.readlines()
                text2 = f2.readlines()
                self.assertEqual(text1, text2)
                f1.close()
                f2.close()

if __name__ == '__main__':
    unittest.main()