/** main.swift
    Contains the entire Projectile program
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

extension String: Error {}

/** Calculates flight duration: the time when the projectile lands (s)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g_vect: gravitational acceleration (m/s^2)
    - Returns: flight duration: the time when the projectile lands (s)
*/
func func_t_flight(_ v_launch: Double, _ theta: Double, _ g_vect: Double) -> Double {
    return 2.0 * v_launch * sin(theta) / g_vect
}

/** Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter g_vect: gravitational acceleration (m/s^2)
    - Returns: landing position: the distance from the launcher to the final position of the projectile (m)
*/
func func_p_land(_ v_launch: Double, _ theta: Double, _ g_vect: Double) -> Double {
    return 2.0 * pow(v_launch, 2.0) * sin(theta) * cos(theta) / g_vect
}

/** Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter p_land: landing position: the distance from the launcher to the final position of the projectile (m)
    - Returns: distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
func func_d_offset(_ p_target: Double, _ p_land: Double) -> Double {
    return p_land - p_target
}

/** Calculates output message as a string
    - Parameter p_target: target position: the distance from the launcher to the target (m)
    - Parameter epsilon: hit tolerance
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Returns: output message as a string
*/
func func_s(_ p_target: Double, _ epsilon: Double, _ d_offset: Double) -> String {
    if abs(d_offset / p_target) < epsilon {
        return "The target was hit."
    }
    else if d_offset < 0.0 {
        return "The projectile fell short."
    }
    else {
        return "The projectile went long."
    }
}

/** Reads input from a file with the given file name
    - Parameter filename: name of the input file
    - Returns: launch speed: the initial speed of the projectile when launched (m/s)
    - Returns: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Returns: target position: the distance from the launcher to the target (m)
*/
func get_input(_ filename: String) throws -> (Double, Double, Double) {
    var v_launch: Double
    var theta: Double
    var p_target: Double
    
    var infile: URL
    infile = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent(filename)
    var goolContents: [[String]]
    do {
        goolContents = try String(contentsOf: infile).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
    } catch {
        throw "Error reading from file."
    }
    v_launch = Double(goolContents[1][0])!
    theta = Double(goolContents[2][0])!
    p_target = Double(goolContents[3][0])!
    
    return (v_launch, theta, p_target)
}

/** Verifies that input values satisfy the physical constraints
    - Parameter v_launch: launch speed: the initial speed of the projectile when launched (m/s)
    - Parameter theta: launch angle: the angle between the launcher and a straight line from the launcher to the target (rad)
    - Parameter p_target: target position: the distance from the launcher to the target (m)
*/
func input_constraints(_ v_launch: Double, _ theta: Double, _ p_target: Double) -> Void {
    if !(v_launch > 0.0) {
        print("Warning: ", terminator: "")
        print("v_launch has value ", terminator: "")
        print(v_launch, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
    }
    if !(0.0 < theta && theta < Double.pi / 2.0) {
        print("Warning: ", terminator: "")
        print("theta has value ", terminator: "")
        print(theta, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("between ", terminator: "")
        print(0.0, terminator: "")
        print(" and ", terminator: "")
        print(Double.pi / 2.0, terminator: "")
        print(" ((pi)/(2))", terminator: "")
        print(".")
    }
    if !(p_target > 0.0) {
        print("Warning: ", terminator: "")
        print("p_target has value ", terminator: "")
        print(p_target, terminator: "")
        print(", but is suggested to be ", terminator: "")
        print("above ", terminator: "")
        print(0.0, terminator: "")
        print(".")
    }
}

/** Writes the output values to output.txt
    - Parameter s: output message as a string
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter t_flight: flight duration: the time when the projectile lands (s)
*/
func write_output(_ s: String, _ d_offset: Double, _ t_flight: Double) throws -> Void {
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

var filename: String = CommandLine.arguments[0]
var g_vect: Double = 9.8
var epsilon: Double = 2.0e-2
var v_launch: Double
var theta: Double
var p_target: Double
(v_launch, theta, p_target) = try get_input(filename)
input_constraints(v_launch, theta, p_target)
var t_flight: Double = func_t_flight(v_launch, theta, g_vect)
var p_land: Double = func_p_land(v_launch, theta, g_vect)
var d_offset: Double = func_d_offset(p_target, p_land)
var s: String = func_s(p_target, epsilon, d_offset)
try write_output(s, d_offset, t_flight)
