module FileTests

global fileToWrite = open("testText.txt", "w")
print(fileToWrite, 0)
println(fileToWrite, 0.89)
print(fileToWrite, "ello")
println(fileToWrite, "bye")
println(fileToWrite, "!!")
close(fileToWrite)
global fileToRead = open("testText.txt", "r")
global fileLine = readline(fileToRead)
readline(fileToRead)
global fileContents = []

global fileContents = readlines(fileToRead)

println(fileContents)
close(fileToRead)

end
