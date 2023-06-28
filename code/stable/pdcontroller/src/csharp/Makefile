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

# Project Name and Purpose: PD_Controller - To provide a model of a PD Controller that can be used for the tuning of the gain constants before the deployment of the controller.

build: PD_Controller$(TARGET_EXTENSION)

PD_Controller$(TARGET_EXTENSION): Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	$(CSC) -out:PD_Controller$(TARGET_EXTENSION) -r:Microsoft.Research.Oslo.dll Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs

run: build
	./PD_Controller$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	doxygen doxConfig

.PHONY: build run doc
