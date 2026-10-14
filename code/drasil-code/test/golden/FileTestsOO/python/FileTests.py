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
assert fileLine != "", "First line should not be empty."
fileContents = []

fileContents = fileToRead.readlines()

print(fileContents)
assert len(fileContents) > 0, "fileContents should not be empty."
fileToRead.close()
