/** ReadTable.swift
    Provides a function for reading glass ASTM data
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Reads glass ASTM data from a file with the given file name
    - Parameter filename: name of the input file
    - Parameter z_vector: list of z values
    - Parameter x_matrix: lists of x values at different z values
    - Parameter y_matrix: lists of y values at different z values
*/
func read_table(_ filename: String, _ z_vector: [Double], _ x_matrix: [[Double]], _ y_matrix: [[Double]]) -> Void {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function read_table called with inputs: {".utf8))
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
        try outfile.write(contentsOf: Data("  z_vector = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(z_vector.description.utf8))
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
        try outfile.write(contentsOf: Data("  x_matrix = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(x_matrix.description.utf8))
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
        try outfile.write(contentsOf: Data("  y_matrix = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(y_matrix.description.utf8))
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
    
    var infile: URL
    var line: String
    var linetokens: [String] = []
    var lines: [String] = []
    infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
    var contents: [[String]]
    do {
        contents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
    } catch {
        throw "Error reading from file."
    }
    var line: [String]
    line = [Int](stride(from: 0, to: contents[0].count, by: 1)).map({(i: Int) -> String in contents[0][i]})
    line = line.joined(separator: " ")
    linetokens = line.components(separatedBy: ",")
    for stringlist_i in [Int](stride(from: 0, to: linetokens.count / 1, by: 1)) {
        z_vector.append(Double(linetokens[stringlist_i * 1 + 0])!)
    }
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'z_vector' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(z_vector.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module ReadTable".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    contents = [Int](stride(from: 2, to: contents.count, by: 1)).map({(i: Int) -> [String] in contents[i]})
    lines = contents.map({(l: [String]) -> String in l.joined(separator: " ")})
    for i in [Int](stride(from: 0, to: lines.count, by: 1)) {
        linetokens = lines[i].components(separatedBy: ",")
        var x_matrix_temp: [Double] = []
        var y_matrix_temp: [Double] = []
        for stringlist_i in [Int](stride(from: 0, to: linetokens.count / 2, by: 1)) {
            x_matrix_temp.append(Double(linetokens[stringlist_i * 2 + 0])!)
            y_matrix_temp.append(Double(linetokens[stringlist_i * 2 + 1])!)
        }
        x_matrix.append(x_matrix_temp)
        y_matrix.append(y_matrix_temp)
    }
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'x_matrix' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(x_matrix.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module ReadTable".utf8))
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
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'y_matrix' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(y_matrix.description.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module ReadTable".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
}
