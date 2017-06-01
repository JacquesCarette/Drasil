"""
Interpolation Module
Secret: The interpolation algorithm.
Service: Provides the equations that take the input parameters and interpolation
data and return an interpolated value.
"""


class BoundError(Exception):
    pass

def lin_interp(x1, y1, x2, y2, x):
    y = (y2 - y1)/(x2 - x1)*(x - x1) + y1
    return y

    
def indInSeq(arr, v):
    for i in range(len(arr) - 1):
        if arr[i] <= v and v <= arr[i+1]:
            return i
    raise BoundError

def matrixCol(mat, c):
    col = []
    for i in range(len(mat)):
        col.append(mat[i][c])
    return col
    
def interpY(x_array, y_array, z_array, x, z):
    # find index that bounds z
    i = indInSeq(z_array, z)
    
    # x and y values for the z curves at i and i+1 (z_1 and z_2)
    x_z_1 = matrixCol(x_array, i)
    y_z_1 = matrixCol(y_array, i)
    x_z_2 = matrixCol(x_array, i + 1)
    y_z_2 = matrixCol(y_array, i + 1)
        
    # find indices that bound x for each z curve
    try:
        j = indInSeq(x_z_1, x)
        k = indInSeq(x_z_2, x)
    except BoundError:
        raise SystemExit("Interpolation of y failed")
        
    # interpolate y values on z curves z_1 and z_2 at x
    y_1 = lin_interp(x_z_1[j], y_z_1[j], x_z_1[j+1], y_z_1[j+1], x)
    y_2 = lin_interp(x_z_2[k], y_z_2[k], x_z_2[k+1], y_z_2[k+1], x)

    # interpolate y
    return lin_interp(z_array[i], y_1, z_array[i+1], y_2, z)
            
        
def interpZ(x_array, y_array, z_array, x, y):
    for i in range(len(z_array) - 1):
        # x and y values for two consecutive z curves 
        x_z_1 = matrixCol(x_array, i)
        y_z_1 = matrixCol(y_array, i)
        x_z_2 = matrixCol(x_array, i + 1)
        y_z_2 = matrixCol(y_array, i + 1)
        
        # find indices that bound x for each z curve
        try:
            j = indInSeq(x_z_1, x)
            k = indInSeq(x_z_2, x)
        except BoundError:
            continue
        
        # interpolate y values at x on z curves
        y_lower = lin_interp(x_z_1[j], y_z_1[j], x_z_1[j+1], y_z_1[j+1], x)
        y_upper = lin_interp(x_z_2[k], y_z_2[k], x_z_2[k+1], y_z_2[k+1], x)
        
        # check if y is bound
        if y_lower <= y and y <= y_upper:
            # interpolate z
            return lin_interp(y_lower, z_array[i], y_upper, z_array[i+1], y)
            
    raise SystemExit("Interpolation of z failed")