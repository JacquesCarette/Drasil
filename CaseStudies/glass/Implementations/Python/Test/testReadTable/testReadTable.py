import os
import unittest
from Implementation import readTable
import numpy as np

class TestReadTable(unittest.TestCase):
   
   def setUp(self):
        pass

   def test_read_table(self):
       num_col, array1, array2 = readTable.read_table(os.path.join("Test/Inputfiles", "testTable1.txt"))
       self.assertTrue((num_col == np.array([ 4.5, 9.1, 14., 18. ])).all())
       self.assertTrue((array1 == np.array([[6.143397364345, 6.128184215473, 6.344639409554, 7.199850837298], 
                                            [6.158648279689, 6.158648279689, 6.344639409554, 7.21772438659], 
                                            [6.189263785047, 6.189263785047, 6.376179502933, 7.253604707984], 
                                            [6.189263785047, 6.189263785047, 6.407876386546, 7.271611700659], 
                                            [6.204628563268, 6.22003148438, 6.407876386546, 7.289663395493]])).all())
       self.assertTrue((array2 == np.array([[4.411877630513, 7.62703125734, 9.952136903603, 9.992650492472], 
                                            [4.388319475839, 7.575020192327, 10.00554408521, 9.924527029632], 
                                            [4.364878545185, 7.494339327108, 9.898975926537, 9.871513535251], 
                                            [4.341579876052, 7.534556975369, 9.793542815831, 9.833410793036], 
                                            [4.318397091121, 7.434385814135, 9.846098917578, 9.766372924029]])).all())
    
if __name__ == "__main__":
    unittest.main()
