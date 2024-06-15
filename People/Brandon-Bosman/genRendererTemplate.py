## Purpose: generates a template with unimplemented instances of all the classes
#           in the target file.
#  Instructions: Modify the strings under # Inputs: inFilePath, outFilePath, oldName, newName.
#                  inFilePath should be a path to an existing renderer (e.g. SwiftRenderer.hs)
#                  oldName should be the name of the main type for the existing renderer (e.g. SwiftRenderer)
#                  newFilePath and newName should be pretty self-explanatory.
#                Then run the file with your python3 interpreter.
#  Created by Brandon Bosman
#  Date: May 16, 2024

# Inputs
inFilePath = "drasil-gool/lib/GOOL/Drasil/LanguageRenderer/SwiftRenderer.hs"
outFilePath = "drasil-gool/lib/GOOL/Drasil/LanguageRenderer/TemplateJuliaRenderer.hs"
oldName = "SwiftCode"
newName = "JuliaCode"

# The datatype for an instance: it has a declaration, some types, and some methods
def newInstance(line: str):
    return {
        "declaration" : line.replace(oldName, newName),
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

    # Detect an instance declaration
    if line.startswith("instance"):
        inInstance = True
        instances.append(newInstance(line))
        continue
    
    if inInstance:

        # Unindented code means the current instance is finished
        if not line.isspace() and not line.startswith("  "):
            inInstance = False
            print("Not instance: " + line)
            continue

        # Skim over blank lines and lines indented more than once
        if line.isspace():
            print("All space: " + line)
            continue
        
        if len(line) > 2 and line[2] == " ":
            print("Nested indent: " + line)
            continue

        start = 2 # the first character after the indent

        # Add types and methods
        if line[start:].startswith("type"):
            instances[-1]["types"].append(line.replace(oldName, newName))
        else:
            # Add everything before the 'main' = sign ('main' determined by 
            # brackets), then add undefined after the = sign.
            end = start + 1
            brackets = 0
            while end <= len(line) and (line[end-1] != "=" or brackets != 0):
                if line[end-1] == "(":
                    brackets += 1
                elif line[end-1] == ")":
                    brackets -= 1
                end += 1
            instances[-1]["methods"].append(line[0:end] + " undefined")

# Output findings
with open(outFilePath, "w") as outFile:
    outFile.write("")

with open(outFilePath, "a") as outFile:
    for instance in instances:
        outFile.write(instance["declaration"])
        for type in instance["types"]:
            outFile.write(type + "\n")
        for method in instance["methods"]:
            outFile.write(method + "\n")
        outFile.write("\n")
