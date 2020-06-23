import Foundation

extension String: Error {}

var fileToWrite: FileHandle
do {
    fileToWrite = try FileHandle(forWritingTo: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("testText.txt"))
} catch {
    throw "Error opening file."
}
do {
    try fileToWrite.write(contentsOf: Data(String(0).utf8))
} catch {
    throw "Error printing to file."
}
do {
    try fileToWrite.write(contentsOf: Data(String(0.89).utf8))
    try fileToWrite.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try fileToWrite.write(contentsOf: Data("ello".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try fileToWrite.write(contentsOf: Data("bye".utf8))
    try fileToWrite.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try fileToWrite.write(contentsOf: Data("!!".utf8))
    try fileToWrite.write(contentsOf: Data("\n".utf8))
} catch {
    throw "Error printing to file."
}
do {
    try fileToWrite.close()
} catch {
    throw "Error closing file."
}
var fileToRead: URL
fileToRead = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("testText.txt")
var fileLine: String
// A statement to read a line from a file should be here, but this is not yet implemented.
// A statement to skip over a line from a file should be here, but this is not yet implemented.
var fileContents: [String] = []

do {
    fileContents = try String(contentsOf: fileToRead).split(separator: "\n").map({(i: Substring) -> String in String(i)})
} catch {
    print("error reading from file", terminator: "")
}

print(fileContents)
