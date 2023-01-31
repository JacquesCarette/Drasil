/** OutputFormat.swift
    Provides the function for writing outputs
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

/** Writes the output values to output.txt
    - Parameter s: output message as a string
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter t_flight: flight duration: the time when the projectile lands (s)
*/
func write_output(_ s: String, _ d_offset: Float, _ t_flight: Float) throws -> Void {
    var outputfile: FileHandle
    do {
        outputfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("output.txt"))
    } catch {
        throw "Error opening file."
    }
    do {
        try outputfile.write(contentsOf: Data("s = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(s.utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("d_offset = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(d_offset).utf8))
        try outputfile.write(contentsOf: Data("\n".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data("t_flight = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outputfile.write(contentsOf: Data(String(t_flight).utf8))
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
