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
var goolContents: [[String]]
do {
    goolContents = try String(contentsOf: fileToRead).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
} catch {
    throw "Error reading from file."
}
var fileLine: String
fileLine = goolContents[0][0]
var fileContents: [String] = []

goolContents = [Int](stride(from: 2, to: goolContents.count, by: 1)).map({(i: Int) -> [String] in goolContents[i]})
fileContents = goolContents.map({(l: [String]) -> String in l.joined(separator: " ")})

print(fileContents)
