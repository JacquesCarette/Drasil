/** DerivedValues.swift
    Provides the function for calculating derived values
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Calculates values that can be immediately derived from the inputs
    - Parameter inParams: structure holding the input values
*/
func derived_values(_ inParams: InputParameters) throws -> Void {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function derived_values called with inputs: {".utf8))
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
    
    inParams.LDF = pow(3.0 / 60.0, 7.0 / 16.0)
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.LDF' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.LDF).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module DerivedValues".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    inParams.SD = sqrt(pow(inParams.SD_x, 2.0) + pow(inParams.SD_y, 2.0) + pow(inParams.SD_z, 2.0))
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.SD' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.SD).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module DerivedValues".utf8))
        try outfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.close()
    } catch {
        throw "Error closing file."
    }
    
    inParams.w_TNT = inParams.w * inParams.TNT
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("var 'inParams.w_TNT' assigned ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(inParams.w_TNT).utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(" in module DerivedValues".utf8))
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
