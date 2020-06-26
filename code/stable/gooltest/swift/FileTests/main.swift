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
var contents: [[String]]
do {
    contents = try String(contentsOf: fileToRead).components(separatedBy: "\n").map({(l: String) -> [String] in l.components(separatedBy: " ")})
} catch {
    throw "Error reading from file."
}
var fileLine: String
var line: [String]
line = [Int](stride(from: 0, to: contents[0].count, by: 1)).map({(i: Int) -> String in contents[0][i]})
fileLine = line.joined(separator: " ")
var fileContents: [String] = []

contents = [Int](stride(from: 3, to: contents.count, by: 1)).map({(i: Int) -> [String] in contents[i]})
fileContents = contents.map({(l: [String]) -> String in l.joined(separator: " ")})

print(fileContents)
