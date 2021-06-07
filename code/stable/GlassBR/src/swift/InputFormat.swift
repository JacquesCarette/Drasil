/** InputFormat.swift
    Provides the function for reading inputs
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Reads input from a file with the given file name
    - Parameter filename: name of the input file
    - Parameter inParams: structure holding the input values
*/
func get_input(_ filename: String, _ inParams: InputParameters) throws -> Void {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function get_input called with inputs: {".utf8))
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
        try outfile.write(contentsOf: Data("  inParams = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data("Instance of InputParameters object".utf8))
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
    infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
    var goolContents: [[String]]
    do {
        goolContents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
    } catch {
        throw "Error reading from file."
    }
    inParams.a = Double(goolContents[1][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.a' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.a).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.b = Double(goolContents[2][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.b' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.b).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.w = Double(goolContents[3][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.w' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.w).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.P_btol = Double(goolContents[4][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.P_btol' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.P_btol).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.TNT = Double(goolContents[5][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.TNT' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.TNT).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.g = goolContents[6][0]
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.g' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(inParams.g.utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.t = Double(goolContents[7][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.t' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.t).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.SD_x = Double(goolContents[8][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.SD_x' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.SD_x).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.SD_y = Double(goolContents[9][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.SD_y' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.SD_y).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    inParams.SD_z = Double(goolContents[10][0])!
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.SD_z' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.SD_z).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module InputFormat".utf8))
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
