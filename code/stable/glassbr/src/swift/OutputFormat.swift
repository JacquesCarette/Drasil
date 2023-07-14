/** OutputFormat.swift
    Provides the function for writing outputs
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Writes the output values to output.txt
    - Parameter inParams: structure holding the input values
    - Parameter isSafePb: Safety Req-Pb
    - Parameter isSafeLR: Safety Req-LR
    - Parameter B: risk of failure
    - Parameter J: stress distribution factor (Function)
    - Parameter NFL: non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    - Parameter q_hat: dimensionless load
    - Parameter q_hat_tol: tolerable load
    - Parameter J_tol: stress distribution factor (Function) based on Pbtol
*/
func write_output(_ inParams: inout InputParameters, _ isSafePb: Bool, _ isSafeLR: Bool, _ B: Double, _ J: Double, _ NFL: Double, _ q_hat: Double, _ q_hat_tol: Double, _ J_tol: Double) throws -> Void {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function write_output called with inputs: {".utf8))
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
        try outfile.write(contentsOf: Data("  isSafePb = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(isSafePb).utf8))
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
        try outfile.write(contentsOf: Data("  isSafeLR = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(isSafeLR).utf8))
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
        try outfile.write(contentsOf: Data("  B = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(B).utf8))
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
        try outfile.write(contentsOf: Data("  J = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(J).utf8))
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
        try outfile.write(contentsOf: Data("  NFL = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(NFL).utf8))
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
        try outfile.write(contentsOf: Data("  q_hat = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(q_hat).utf8))
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
        try outfile.write(contentsOf: Data("  q_hat_tol = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(q_hat_tol).utf8))
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
        try outfile.write(contentsOf: Data("  J_tol = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(J_tol).utf8))
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
    
    var outputfile: FileHandle
    do {
        outputfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("output.txt"))
    } catch {
        throw "Error opening file."
    }
    do {
        try outputfile.write(contentsOf: Data("isSafePb = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(isSafePb).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("isSafeLR = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(isSafeLR).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("B = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(B).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("J = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(J).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("NFL = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(NFL).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("GTF = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(inParams.GTF).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("q_hat = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(q_hat).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("q_hat_tol = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(q_hat_tol).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("J_tol = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(J_tol).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("h = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(inParams.h).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("AR = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(inParams.AR).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.close()
    } catch {
        throw "Error closing file."
    }
}
