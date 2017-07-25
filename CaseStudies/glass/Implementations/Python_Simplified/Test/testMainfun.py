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
            
        
    def test_main(self):
        for i in range(self.numTests):
            with self.subTest(i=i):
                mainfun.main(os.path.join("Test/Inputfiles", self.inputFileName[i]))
                with open("outputfile.txt", 'r') as f:
                    text1 = f.readlines()
                with open(os.path.join("Test/Inputfiles", self.outputFileName[i]), 'r') as f:
                    text2 = f.readlines()
                self.assertEqual(text1, text2)
  

if __name__ == '__main__':
    unittest.main()