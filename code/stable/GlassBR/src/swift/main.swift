/** main.swift
    Controls the flow of the program
    - Authors: Nikitha Krithnan and W. Spencer Smith
*/
import Foundation

extension String: Error {}

var outfile: FileHandle
var filename: String = CommandLine.arguments[0]
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'filename' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(filename.utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var inParams: InputParameters = InputParameters()
try get_input(filename, inParams)
try derived_values(inParams)
try input_constraints(&inParams)
var J_tol: Double = try func_J_tol(&inParams)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'J_tol' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(J_tol).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var q: Double = try func_q(&inParams)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'q' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(q).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var q_hat: Double = try func_q_hat(&inParams, q)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'q_hat' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(q_hat).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var q_hat_tol: Double = try func_q_hat_tol(&inParams, J_tol)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'q_hat_tol' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(q_hat_tol).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var J: Double = try func_J(&inParams, q_hat)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'J' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(J).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var NFL: Double = try func_NFL(&inParams, q_hat_tol)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'NFL' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(NFL).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var B: Double = try func_B(&inParams, J)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'B' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(B).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var LR: Double = try func_LR(&inParams, NFL)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'LR' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(LR).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var is_safeLR: Bool = try func_is_safeLR(LR, q)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'is_safeLR' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(is_safeLR).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var P_b: Double = try func_P_b(B)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'P_b' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(P_b).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var is_safePb: Bool = try func_is_safePb(&inParams, P_b)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'is_safePb' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(is_safePb).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Control".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
try write_output(is_safePb, is_safeLR, P_b, J)
