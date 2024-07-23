module FileTests

fileToWrite = open("testText.txt", "w")
print(fileToWrite, 0)
println(fileToWrite, 0.89)
print(fileToWrite, "ello")
println(fileToWrite, "bye")
println(fileToWrite, "!!")
close(fileToWrite)
fileToRead = open("testText.txt", "r")
fileLine = readline(fileToRead)
readline(fileToRead)
fileContents = []

fileContents = readlines(fileToRead)

println(fileContents)
close(fileToRead)

end
