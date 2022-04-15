/** InputFormat.swift
    Provides the function for reading inputs
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

extension String: Error {}

/** Reads input from a file with the given file name
    - Parameter filename: name of the input file
    - Returns: launch speed: the initial speed of the projectile when launched (m/s)
    - Returns: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Returns: target position: the distance from the launcher to the target (m)
*/
func get_input(_ filename: String) throws -> (Float, Float, Float) {
    var v_launch: Float
    var theta: Float
    var p_target: Float
    
    var infile: URL
    infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
    var goolContents: [[String]]
    do {
        goolContents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
    } catch {
        throw "Error reading from file."
    }
    v_launch = Float(goolContents[1][0])!
    theta = Float(goolContents[2][0])!
    p_target = Float(goolContents[3][0])!
    
    return (v_launch, theta, p_target)
}
