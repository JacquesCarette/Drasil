ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
    CSC=csc
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
        CSC=mcs
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
        CSC=mcs
    endif
endif

build: GlassBR$(TARGET_EXTENSION)

GlassBR$(TARGET_EXTENSION): Control.cs InputParameters.cs DerivedValues.cs InputConstraints.cs InputFormat.cs OutputFormat.cs Calculations.cs ReadTable.cs Interpolation.cs
	$(CSC) -out:GlassBR$(TARGET_EXTENSION) Control.cs InputParameters.cs DerivedValues.cs InputConstraints.cs InputFormat.cs OutputFormat.cs Calculations.cs ReadTable.cs Interpolation.cs

run: build
	./GlassBR$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
