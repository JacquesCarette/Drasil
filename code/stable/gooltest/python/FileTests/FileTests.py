fileToWrite = open("testText.txt", "w")
print(0, end="", file=fileToWrite)
print(0.89, file=fileToWrite)
print("ello", end="", file=fileToWrite)
print("bye", file=fileToWrite)
print("!!", file=fileToWrite)
fileToWrite.close()
fileToRead = open("testText.txt", "r")
fileLine = fileToRead.readline().rstrip()
fileToRead.readline()
fileContents = []

fileContents = fileToRead.readlines()

print(fileContents)
fileToRead.close()
