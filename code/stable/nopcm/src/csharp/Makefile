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

# Project Name and Purpose: NoPCM - To investigate the heating of water in a solar water heating tank.

build: NoPCM$(TARGET_EXTENSION)

NoPCM$(TARGET_EXTENSION): Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	$(CSC) -out:NoPCM$(TARGET_EXTENSION) -r:Microsoft.Research.Oslo.dll Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs

run: build
	./NoPCM$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	doxygen doxConfig

.PHONY: build run doc
