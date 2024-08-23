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
@assert fileLine != "" "First line should not be empty."
global fileContents = String[]

global fileContents = readlines(fileToRead)

println(fileContents)
@assert length(fileContents) > 0 "fileContents should not be empty."
close(fileToRead)

end
