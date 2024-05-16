## Purpose: generates a template with unimplemented instances of all the classes
#           in the target file.
#  Created by Brandon Bosman
#  Date: May 16, 2024

# Inputs
inFilePath = "drasil-gool/lib/GOOL/Drasil/LanguageRenderer/SwiftRenderer.hs"
outFilePath = "drasil-gool/lib/GOOL/Drasil/LanguageRenderer/TemplateJuliaRenderer.hs"
oldName = "SwiftCode"
newName = "JuliaCode"

def newInstance(line: str):
    return {
        "top" : line.replace(oldName, newName),
        "types" : [], 
        "methods" : []
    }

# Read file
with open(inFilePath, "r") as inFile:
    lines = inFile.readlines()

# Go through lines and find instances of typeclasses
instances = []
inInstance = False

for line in lines:
    if line.startswith("instance"):
        inInstance = True
        instances.append(newInstance(line))
        continue
    
    if inInstance:
        if not line.isspace() and not line.startswith("  "):
            inInstance = False
            print("Not instance: " + line)
            continue

        if line.isspace():
            print("All space: " + line)
            continue
        
        if len(line) > 2 and line[2] == " ":
            print("Nested indent: " + line)
            continue

        start = 2
        # while start < len(line)-1 and line[start] == " ":
        #     start += 1

        if line[start:].startswith("type"):
            instances[-1]["types"].append(line.replace(oldName, newName))
        else:
            end = start + 1
            brackets = 0
            while end <= len(line) and (line[end-1] != "=" or brackets != 0):
                if line[end-1] == "(":
                    brackets += 1
                elif line[end-1] == ")":
                    brackets -= 1
                end += 1
            instances[-1]["methods"].append(line[0:end] + " undefined")

with open(outFilePath, "w") as outFile:
    outFile.write("")

with open(outFilePath, "a") as outFile:
    for instance in instances:
        outFile.write(instance["top"])
        for type in instance["types"]:
            outFile.write(type + "\n")
        for method in instance["methods"]:
            outFile.write(method + "\n")
        outFile.write("\n")