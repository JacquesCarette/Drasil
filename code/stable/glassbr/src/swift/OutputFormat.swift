/** OutputFormat.swift
    Provides the function for writing outputs
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

/** Writes the output values to output.txt
    - Parameter inParams: structure holding the input values
    - Parameter B: risk of failure
    - Parameter J: stress distribution factor (Function)
    - Parameter NFL: non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    - Parameter q_hat: dimensionless load
    - Parameter q_hat_tol: tolerable load
    - Parameter J_tol: stress distribution factor (Function) based on Pbtol
    - Parameter P_b: probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    - Parameter LR: load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    - Parameter isSafePb: Safety Req-Pb
    - Parameter isSafeLR: Safety Req-LR
*/
func write_output(_ inParams: inout InputParameters, _ B: Double, _ J: Double, _ NFL: Double, _ q_hat: Double, _ q_hat_tol: Double, _ J_tol: Double, _ P_b: Double, _ LR: Double, _ isSafePb: Bool, _ isSafeLR: Bool) throws -> Void {
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
        try outfile.write(contentsOf: Data("  P_b = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(P_b).utf8))
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
        try outfile.write(contentsOf: Data("  LR = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(LR).utf8))
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
        try outputfile.write(contentsOf: Data("P_b = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(P_b).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("LR = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(LR).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
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
