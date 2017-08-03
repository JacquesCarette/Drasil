import os
import unittest
from Implementation import param
from Implementation import inputFormat

class TestInputFormat(unittest.TestCase):
 
    def setUp(self):
        with open(os.path.join("Test/Inputfiles", "inputFormat.txt"), 'r') as f:
            input = f.readlines()
        input = list(map(lambda x: x.split(","), input))
        self.numTests = len(input)
        self.params           = [param.Param() for i in range(self.numTests)]

        self.inputFileName    = [row[0]        for row in input]
        self.aExpctd          = [float(row[1]) for row in input]
        self.bExpctd          = [float(row[2]) for row in input]
        self.tExpctd          = [float(row[3]) for row in input]
        self.gtExpctd         = [row[4]        for row in input]
        self.wExpctd          = [float(row[5]) for row in input]
        self.tntExpctd        = [float(row[6]) for row in input]
        self.sdxExpctd        = [float(row[7]) for row in input]
        self.sdyExpctd        = [float(row[8]) for row in input]
        self.sdzExpctd        = [float(row[9]) for row in input]
        self.pbTolExpctd      = [float(row[10].rstrip()) for row in input]

        for i in range(self.numTests):
            inputFormat.get_input(os.path.join("Test/Inputfiles", self.inputFileName[i]), self.params[i])

    def test_get_input(self):
        for i in range(self.numTests):
            with self.subTest(i=i):

                self.assertEqual(self.aExpctd[i], self.params[i].a) 
                self.assertEqual(self.bExpctd[i], self.params[i].b)
                self.assertEqual(self.tExpctd[i], float(self.params[i].t))
                self.assertEqual(self.gtExpctd[i], self.params[i].gt)
                self.assertEqual(self.wExpctd[i], self.params[i].w)
                self.assertEqual(self.tntExpctd[i], self.params[i].tnt)
                self.assertEqual((self.sdxExpctd[i], self.sdyExpctd[i], self.sdzExpctd[i]), self.params[i].sdvect)
                self.assertEqual(self.pbTolExpctd[i], self.params[i].pbtol)
        
if __name__ == '__main__':
    unittest.main()