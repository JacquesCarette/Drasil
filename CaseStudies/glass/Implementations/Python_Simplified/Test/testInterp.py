import unittest
import numpy as np
from Implementation import interp

class TestInterp(unittest.TestCase):
    
    def setUp(self):
        self.data1 = np.array([4.5, 9.1, 18.0, 140, 450, 910])
        self.data2 = np.array([[1.1, 2.2, 3.3, 4.4, 5.5, 6.6],
                               [1.2, 2.3, 3.4, 4.4, 5.6, 6.7],
                               [1.3, 2.3, 3.5, 4.6, 5.7, 6.8],
                               [1.4, 2.5, 3.6, 4.7, 5.8, 6.9],
                               [1.5, 2.6, 3.7, 4.8, 5.9, 7.0],
                               [1.6, 2.7, 3.8, 4.8, 6.0, 7.1],
                               [1.7, 2.8, 3.9, 5.0, 6.1, 7.3],
                               [2.3, 3.5, 4.0, 5.6, 6.3, 7.4],
                               [2.5, 3.6, 4.8, 5.7, 6.3, 7.5],
                               [2.5, 3.7, 4.9, 5.8, 6.4, 7.6]])
        self.data3 = np.array([[2.1, 3.1, 4.1, 5.1, 6.1, 7.1],
                               [2.2, 3.2, 4.2, 5.2, 6.2, 7.2],
                               [2.3, 3.3, 4.3, 5.3, 6.3, 7.3],
                               [2.4, 3.4, 4.4, 5.4, 6.4, 7.4],
                               [2.5, 3.5, 4.5, 5.5, 6.5, 7.5],
                               [2.6, 3.6, 4.6, 5.6, 6.6, 7.6],
                               [2.7, 3.7, 4.7, 5.7, 6.7, 7.7],
                               [2.8, 3.8, 4.8, 5.8, 6.8, 7.8],
                               [2.9, 3.9, 4.9, 5.9, 6.9, 7.9],
                               [3.0, 4.0, 5.0, 6.0, 7.0, 8.0]])
                
    def test_lin_interp(self):
        y = interp.lin_interp(x1, y1, x2, y2, x)
        self.assertEqual(y, yExpctd)

    def test_find_bounds(self):
        idx, jdx, kdx, num_interp1, num_interp2 = interp.find_bounds(self.data1, self.data2, 30, 4.8)
        self.assertEqual(idx, idxShouldBe)
        self.assertEqual(jdx, jdxShouldBe)
        self.assertEqual(kdx, kdxShouldBe)
        self.assertEqual(num_interp1, num_interp1ShouldBe)
        self.assertEqual(num_interp2, num_interp2ShouldBe)

    def test_interp(self):
        interp_value = interp.interp(2, 2, 1, 0, 0, self.data1, self.data2, self.data3, 18, 3.5)
        self.assertEqual(interp_value, interpValExpctd)

if __name__ == "__main__":
    unittest.main()