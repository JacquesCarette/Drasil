## \file Interpolation.py
# \author Nikitha Krithnan and W. Spencer Smith
# \brief Provides functions for linear interpolation on three-dimensional data
import ReadTable

## \brief Performs linear interpolation
# \param x_1 lower x-coordinate
# \param y_1 lower y-coordinate
# \param x_2 upper x-coordinate
# \param y_2 upper y-coordinate
# \param x x-coordinate to interpolate at
# \return y value interpolated at given x value
def lin_interp(x_1, y_1, x_2, y_2, x):
    outfile = open("log.txt", "a")
    print("function lin_interp called with inputs: {", file=outfile)
    print("  x_1 = ", end="", file=outfile)
    print(x_1, end="", file=outfile)
    print(", ", file=outfile)
    print("  y_1 = ", end="", file=outfile)
    print(y_1, end="", file=outfile)
    print(", ", file=outfile)
    print("  x_2 = ", end="", file=outfile)
    print(x_2, end="", file=outfile)
    print(", ", file=outfile)
    print("  y_2 = ", end="", file=outfile)
    print(y_2, end="", file=outfile)
    print(", ", file=outfile)
    print("  x = ", end="", file=outfile)
    print(x, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    return (y_2 - y_1) / (x_2 - x_1) * (x - x_1) + y_1

## \brief Finds the array index for a value closest to the given value
# \param arr array in which value should be found
# \param v value whose index will be found
# \return index of given value in given array
def find(arr, v):
    outfile = open("log.txt", "a")
    print("function find called with inputs: {", file=outfile)
    print("  arr = ", end="", file=outfile)
    print(arr, end="", file=outfile)
    print(", ", file=outfile)
    print("  v = ", end="", file=outfile)
    print(v, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    for i in range(0, len(arr) - 1, 1):
        if (arr[i] <= v and v <= arr[i + 1]) :
            return i
    raise Exception("Bound error")

## \brief Extracts a column from a 2D matrix
# \param mat matrix from which column will be extracted
# \param j index
# \return column of the given matrix at the given index
def extractColumn(mat, j):
    outfile = open("log.txt", "a")
    print("function extractColumn called with inputs: {", file=outfile)
    print("  mat = ", end="", file=outfile)
    print(mat, end="", file=outfile)
    print(", ", file=outfile)
    print("  j = ", end="", file=outfile)
    print(j, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    col = []
    for i in range(0, len(mat), 1):
        col.append(mat[i][j])
    return col

## \brief Linearly interpolates a y value at given x and z values
# \param filename name of file with x y and z data
# \param x x-coordinate to interpolate at
# \param z z-coordinate to interpolate at
# \return y value interpolated at given x and z values
def interpY(filename, x, z):
    outfile = open("log.txt", "a")
    print("function interpY called with inputs: {", file=outfile)
    print("  filename = ", end="", file=outfile)
    print(filename, end="", file=outfile)
    print(", ", file=outfile)
    print("  x = ", end="", file=outfile)
    print(x, end="", file=outfile)
    print(", ", file=outfile)
    print("  z = ", end="", file=outfile)
    print(z, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    x_matrix = []
    y_matrix = []
    z_vector = []
    ReadTable.read_table(filename, z_vector, x_matrix, y_matrix)
    i = find(z_vector, z)
    outfile = open("log.txt", "a")
    print("var 'i' assigned ", end="", file=outfile)
    print(i, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    x_z_1 = extractColumn(x_matrix, i)
    outfile = open("log.txt", "a")
    print("var 'x_z_1' assigned ", end="", file=outfile)
    print(x_z_1, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    y_z_1 = extractColumn(y_matrix, i)
    outfile = open("log.txt", "a")
    print("var 'y_z_1' assigned ", end="", file=outfile)
    print(y_z_1, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    x_z_2 = extractColumn(x_matrix, i + 1)
    outfile = open("log.txt", "a")
    print("var 'x_z_2' assigned ", end="", file=outfile)
    print(x_z_2, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    y_z_2 = extractColumn(y_matrix, i + 1)
    outfile = open("log.txt", "a")
    print("var 'y_z_2' assigned ", end="", file=outfile)
    print(y_z_2, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    try :
        j = find(x_z_1, x)
        outfile = open("log.txt", "a")
        print("var 'j' assigned ", end="", file=outfile)
        print(j, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        k_2 = find(x_z_2, x)
        outfile = open("log.txt", "a")
        print("var 'k_2' assigned ", end="", file=outfile)
        print(k_2, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
    except Exception :
        raise Exception("Interpolation of y failed")
    y_1 = lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
    outfile = open("log.txt", "a")
    print("var 'y_1' assigned ", end="", file=outfile)
    print(y_1, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    y_2 = lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
    outfile = open("log.txt", "a")
    print("var 'y_2' assigned ", end="", file=outfile)
    print(y_2, end="", file=outfile)
    print(" in module Interpolation", file=outfile)
    outfile.close()
    return lin_interp(z_vector[i], y_1, z_vector[i + 1], y_2, z)

## \brief Linearly interpolates a z value at given x and y values
# \param filename name of file with x y and z data
# \param x x-coordinate to interpolate at
# \param y y-coordinate to interpolate at
# \return z value interpolated at given x and y values
def interpZ(filename, x, y):
    outfile = open("log.txt", "a")
    print("function interpZ called with inputs: {", file=outfile)
    print("  filename = ", end="", file=outfile)
    print(filename, end="", file=outfile)
    print(", ", file=outfile)
    print("  x = ", end="", file=outfile)
    print(x, end="", file=outfile)
    print(", ", file=outfile)
    print("  y = ", end="", file=outfile)
    print(y, file=outfile)
    print("  }", file=outfile)
    outfile.close()
    
    x_matrix = []
    y_matrix = []
    z_vector = []
    ReadTable.read_table(filename, z_vector, x_matrix, y_matrix)
    for i in range(0, len(z_vector) - 1, 1):
        x_z_1 = extractColumn(x_matrix, i)
        outfile = open("log.txt", "a")
        print("var 'x_z_1' assigned ", end="", file=outfile)
        print(x_z_1, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        y_z_1 = extractColumn(y_matrix, i)
        outfile = open("log.txt", "a")
        print("var 'y_z_1' assigned ", end="", file=outfile)
        print(y_z_1, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        x_z_2 = extractColumn(x_matrix, i + 1)
        outfile = open("log.txt", "a")
        print("var 'x_z_2' assigned ", end="", file=outfile)
        print(x_z_2, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        y_z_2 = extractColumn(y_matrix, i + 1)
        outfile = open("log.txt", "a")
        print("var 'y_z_2' assigned ", end="", file=outfile)
        print(y_z_2, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        try :
            j = find(x_z_1, x)
            outfile = open("log.txt", "a")
            print("var 'j' assigned ", end="", file=outfile)
            print(j, end="", file=outfile)
            print(" in module Interpolation", file=outfile)
            outfile.close()
            k_2 = find(x_z_2, x)
            outfile = open("log.txt", "a")
            print("var 'k_2' assigned ", end="", file=outfile)
            print(k_2, end="", file=outfile)
            print(" in module Interpolation", file=outfile)
            outfile.close()
        except Exception :
            continue
        y_1 = lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
        outfile = open("log.txt", "a")
        print("var 'y_1' assigned ", end="", file=outfile)
        print(y_1, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        y_2 = lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
        outfile = open("log.txt", "a")
        print("var 'y_2' assigned ", end="", file=outfile)
        print(y_2, end="", file=outfile)
        print(" in module Interpolation", file=outfile)
        outfile.close()
        if (y_1 <= y and y <= y_2) :
            return lin_interp(y_1, z_vector[i], y_2, z_vector[i + 1], y)
    raise Exception("Interpolation of z failed")
