/** main.swift
    Contains the entire Projectile program
    - Authors: Samuel J. Crawford, Brooks MacLachlan, and W. Spencer Smith
*/
import Foundation

extension String: Error {}

/** Structure for holding the input values
*/
class InputParameters {
    var v_launch: Double = 0.0
    var theta: Double = 0.0
    var p_target: Double = 0.0
    
    /** Initializes input object by reading inputs and checking physical constraints on the input
        - Parameter filename: name of the input file
    */
    init(_ filename: String) throws {
        var outfile: FileHandle
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("function InputParameters called with inputs: {".utf8))
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
        
        try self.get_input(filename)
        try self.input_constraints()
    }
    
    /** Reads input from a file with the given file name
        - Parameter filename: name of the input file
    */
    private func get_input(_ filename: String) throws -> Void {
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
        self.v_launch = Double(goolContents[1][0])!
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'self.v_launch' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(self.v_launch).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Projectile".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        self.theta = Double(goolContents[2][0])!
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'self.theta' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(self.theta).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Projectile".utf8))
            try outfile.write(contentsOf: Data("\n".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.close()
        } catch {
            throw "Error closing file."
        }
        self.p_target = Double(goolContents[3][0])!
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("var 'self.p_target' assigned ".utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(String(self.p_target).utf8))
        } catch {
            throw "Error printing to file."
        }
        do {
            try outfile.write(contentsOf: Data(" in module Projectile".utf8))
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
    
    /** Verifies that input values satisfy the physical constraints
    */
    private func input_constraints() throws -> Void {
        var outfile: FileHandle
        do {
            outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
            try outfile.seekToEnd()
        } catch {
            throw "Error opening file."
        }
        do {
            try outfile.write(contentsOf: Data("function input_constraints called with inputs: {".utf8))
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
        
        if !(self.v_launch > 0.0) {
            print("Warning: ", terminator: "")
            print("v_launch has value ", terminator: "")
            print(self.v_launch, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("above ", terminator: "")
            print(0.0, terminator: "")
            print(".")
        }
        if !(0.0 < self.theta && self.theta < Double.pi / 2.0) {
            print("Warning: ", terminator: "")
            print("theta has value ", terminator: "")
            print(self.theta, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("between ", terminator: "")
            print(0.0, terminator: "")
            print(" and ", terminator: "")
            print(Double.pi / 2.0, terminator: "")
            print(" ((pi)/(2))", terminator: "")
            print(".")
        }
        if !(self.p_target > 0.0) {
            print("Warning: ", terminator: "")
            print("p_target has value ", terminator: "")
            print(self.p_target, terminator: "")
            print(", but is suggested to be ", terminator: "")
            print("above ", terminator: "")
            print(0.0, terminator: "")
            print(".")
        }
    }
}

/** Structure for holding the constant values
*/
class Constants {
    static let g: Double = 9.8
    static let epsilon: Double = 2.0e-2
    
}

/** Calculates flight duration: the time when the projectile lands (s)
    - Parameter inParams: structure holding the input values
    - Returns: flight duration: the time when the projectile lands (s)
*/
func func_t_flight(_ inParams: inout InputParameters) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function func_t_flight called with inputs: {".utf8))
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
    
    return 2.0 * inParams.v_launch * sin(inParams.theta) / Constants.g
}

/** Calculates landing position: the distance from the launcher to the final position of the projectile (m)
    - Parameter inParams: structure holding the input values
    - Returns: landing position: the distance from the launcher to the final position of the projectile (m)
*/
func func_p_land(_ inParams: inout InputParameters) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function func_p_land called with inputs: {".utf8))
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
    
    return 2.0 * pow(inParams.v_launch, 2.0) * sin(inParams.theta) * cos(inParams.theta) / Constants.g
}

/** Calculates distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter inParams: structure holding the input values
    - Parameter p_land: landing position: the distance from the launcher to the final position of the projectile (m)
    - Returns: distance between the target position and the landing position: the offset between the target position and the landing position (m)
*/
func func_d_offset(_ inParams: inout InputParameters, _ p_land: Double) throws -> Double {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function func_d_offset called with inputs: {".utf8))
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
        try outfile.write(contentsOf: Data("  p_land = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(p_land).utf8))
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
    
    return p_land - inParams.p_target
}

/** Calculates output message as a string
    - Parameter inParams: structure holding the input values
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Returns: output message as a string
*/
func func_s(_ inParams: inout InputParameters, _ d_offset: Double) throws -> String {
    var outfile: FileHandle
    do {
        outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
        try outfile.seekToEnd()
    } catch {
        throw "Error opening file."
    }
    do {
        try outfile.write(contentsOf: Data("function func_s called with inputs: {".utf8))
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
        try outfile.write(contentsOf: Data("  d_offset = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(d_offset).utf8))
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
    
    if abs(d_offset / inParams.p_target) < Constants.epsilon {
        return "The target was hit."
    }
    else if d_offset < 0.0 {
        return "The projectile fell short."
    }
    else {
        return "The projectile went long."
    }
}

/** Writes the output values to output.txt
    - Parameter s: output message as a string
    - Parameter d_offset: distance between the target position and the landing position: the offset between the target position and the landing position (m)
    - Parameter t_flight: flight duration: the time when the projectile lands (s)
*/
func write_output(_ s: String, _ d_offset: Double, _ t_flight: Double) throws -> Void {
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
        try outfile.write(contentsOf: Data("  s = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(s.utf8))
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
        try outfile.write(contentsOf: Data("  d_offset = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(d_offset).utf8))
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
        try outfile.write(contentsOf: Data("  t_flight = ".utf8))
    } catch {
        throw "Error printing to file."
    }
    do {
        try outfile.write(contentsOf: Data(String(t_flight).utf8))
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
    try outfile.write(contentsOf: Data(" in module Projectile".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var inParams: InputParameters = try InputParameters(filename)
var t_flight: Double = try func_t_flight(&inParams)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 't_flight' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(t_flight).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Projectile".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var p_land: Double = try func_p_land(&inParams)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'p_land' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(p_land).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Projectile".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var d_offset: Double = try func_d_offset(&inParams, p_land)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 'd_offset' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(String(d_offset).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Projectile".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
var s: String = try func_s(&inParams, d_offset)
do {
    outfile = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("log.txt"))
    try outfile.seekToEnd()
} catch {
    throw "Error opening file."
}
do {
    try outfile.write(contentsOf: Data("var 's' assigned ".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(s.utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.write(contentsOf: Data(" in module Projectile".utf8))
    try outfile.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try outfile.close()
} catch {
    throw "Error closing file."
}
try write_output(s, d_offset, t_flight)
