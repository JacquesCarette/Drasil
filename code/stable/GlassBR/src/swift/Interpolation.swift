/** Interpolation.swift
    Provides functions for linear interpolation on three-dimensional data
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Performs linear interpolation
    - Parameter x_1: lower x-coordinate
    - Parameter y_1: lower y-coordinate
    - Parameter x_2: upper x-coordinate
    - Parameter y_2: upper y-coordinate
    - Parameter x: x-coordinate to interpolate at
    - Returns: y value interpolated at given x value
*/
func lin_interp(_ x_1: Double, _ y_1: Double, _ x_2: Double, _ y_2: Double, _ x: Double) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function lin_interp called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  x_1 = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(x_1).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  y_1 = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(y_1).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  x_2 = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(x_2).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  y_2 = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(y_2).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  x = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(x).utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    return (y_2 - y_1) / (x_2 - x_1) * (x - x_1) + y_1
}

/** Finds the array index for a value closest to the given value
    - Parameter arr: array in which value should be found
    - Parameter v: value whose index will be found
    - Returns: index of given value in given array
*/
func find(_ arr: inout [Double], _ v: Double) throws -> Int {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function find called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  arr = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(arr.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  v = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(v).utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    for i in [Int](stride(from: 0, to: arr.count - 1, by: 1)) {
        if arr[i] <= v && v <= arr[i + 1] {
            return i
        }
    }
    throw "Bound error"
}

/** Extracts a column from a 2D matrix
    - Parameter mat: matrix from which column will be extracted
    - Parameter j: index
    - Returns: column of the given matrix at the given index
*/
func extractColumn(_ mat: inout [[Double]], _ j: Int) throws -> [Double] {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function extractColumn called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  mat = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(mat.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  j = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(j).utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    var col: [Double] = []
    for i in [Int](stride(from: 0, to: mat.count, by: 1)) {
        col.append(mat[i][j])
    }
    return col
}

/** Linearly interpolates a y value at given x and z values
    - Parameter filename: name of file with x y and z data
    - Parameter x: x-coordinate to interpolate at
    - Parameter z: z-coordinate to interpolate at
    - Returns: y value interpolated at given x and z values
*/
func interpY(_ filename: String, _ x: Double, _ z: Double) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function interpY called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  filename = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(filename.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  x = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(x).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  z = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(z).utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    var i: Int
    var x_z_1: [Double]
    var y_z_1: [Double]
    var x_z_2: [Double]
    var y_z_2: [Double]
    var j: Int
    var k_2: Int
    var y_1: Double
    var y_2: Double
    
    var x_matrix: [[Double]] = []
    var y_matrix: [[Double]] = []
    var z_vector: [Double] = []
    try read_table(filename, &z_vector, &x_matrix, &y_matrix)
    i = try find(&z_vector, z)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'i' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(i).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    x_z_1 = try extractColumn(&x_matrix, i)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'x_z_1' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(x_z_1.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    y_z_1 = try extractColumn(&y_matrix, i)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'y_z_1' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(y_z_1.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    x_z_2 = try extractColumn(&x_matrix, i + 1)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'x_z_2' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(x_z_2.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    y_z_2 = try extractColumn(&y_matrix, i + 1)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'y_z_2' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(y_z_2.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    do {
        j = try find(&x_z_1, x)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'j' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(j).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        k_2 = try find(&x_z_2, x)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'k_2' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(k_2).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
    } catch {
        throw "Interpolation of y failed"
    }
    y_1 = try lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'y_1' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(y_1).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    y_2 = try lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'y_2' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(y_2).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    return try lin_interp(z_vector[i], y_1, z_vector[i + 1], y_2, z)
}

/** Linearly interpolates a z value at given x and y values
    - Parameter filename: name of file with x y and z data
    - Parameter x: x-coordinate to interpolate at
    - Parameter y: y-coordinate to interpolate at
    - Returns: z value interpolated at given x and y values
*/
func interpZ(_ filename: String, _ x: Double, _ y: Double) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function interpZ called with inputs: {".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  filename = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(filename.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  x = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(x).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(", ".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  y = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(y).utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("  }".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    var x_z_1: [Double]
    var y_z_1: [Double]
    var x_z_2: [Double]
    var y_z_2: [Double]
    var j: Int
    var k_2: Int
    var y_1: Double
    var y_2: Double
    
    var x_matrix: [[Double]] = []
    var y_matrix: [[Double]] = []
    var z_vector: [Double] = []
    try read_table(filename, &z_vector, &x_matrix, &y_matrix)
    for i in [Int](stride(from: 0, to: z_vector.count - 1, by: 1)) {
        x_z_1 = try extractColumn(&x_matrix, i)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'x_z_1' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(x_z_1.description.utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        y_z_1 = try extractColumn(&y_matrix, i)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'y_z_1' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(y_z_1.description.utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        x_z_2 = try extractColumn(&x_matrix, i + 1)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'x_z_2' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(x_z_2.description.utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        y_z_2 = try extractColumn(&y_matrix, i + 1)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'y_z_2' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(y_z_2.description.utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        do {
            j = try find(&x_z_1, x)
            do {
                outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
                try outfile.seekToEnd()
            } catch {
                throw "Error opening file."
            }
            do {
                try outfile.write(contentsOf: Data("var 'j' assigned ".utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.write(contentsOf: Data(String(j).utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
                try outfile.write(contentsOf: Data("\n".utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.close()
            } catch {
                throw "Error closing file."
            }
            k_2 = try find(&x_z_2, x)
            do {
                outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
                try outfile.seekToEnd()
            } catch {
                throw "Error opening file."
            }
            do {
                try outfile.write(contentsOf: Data("var 'k_2' assigned ".utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.write(contentsOf: Data(String(k_2).utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
                try outfile.write(contentsOf: Data("\n".utf8))
            } catch {
                throw "Error printing to file."
            }
            do {
                try outfile.close()
            } catch {
                throw "Error closing file."
            }
        } catch {
            continue
        }
        y_1 = try lin_interp(x_z_1[j], y_z_1[j], x_z_1[j + 1], y_z_1[j + 1], x)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'y_1' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(y_1).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        y_2 = try lin_interp(x_z_2[k_2], y_z_2[k_2], x_z_2[k_2 + 1], y_z_2[k_2 + 1], x)
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'y_2' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(y_2).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Interpolation".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        if y_1 <= y && y <= y_2 {
            return try lin_interp(y_1, z_vector[i], y_2, z_vector[i + 1], y)
        }
    }
    throw "Interpolation of z failed"
}
