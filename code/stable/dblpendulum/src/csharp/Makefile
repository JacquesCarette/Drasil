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

# Project Name and Purpose: DblPendulum - To predict the motion of a pendulum.

build: DblPendulum$(TARGET_EXTENSION)

DblPendulum$(TARGET_EXTENSION): Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	$(CSC) -out:DblPendulum$(TARGET_EXTENSION) -r:Microsoft.Research.Oslo.dll Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs

run: build
	./DblPendulum$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	doxygen doxConfig

.PHONY: build run doc
