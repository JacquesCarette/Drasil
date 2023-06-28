ifeq "$(OS)" "Windows_NT"
    LIB_EXTENSION=.dll
    CSC=csc
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        LIB_EXTENSION=.so
        CSC=mcs
    endif
    ifeq ($(UNAME_S), Darwin)
        LIB_EXTENSION=.dylib
        CSC=mcs
    endif
endif

# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build: Projectile$(LIB_EXTENSION)

Projectile$(LIB_EXTENSION): InputFormat.cs InputConstraints.cs Calculations.cs OutputFormat.cs
	$(CSC) -t:library -out:Projectile$(LIB_EXTENSION) InputFormat.cs InputConstraints.cs Calculations.cs OutputFormat.cs

doc: doxConfig InputFormat.cs InputConstraints.cs Calculations.cs OutputFormat.cs
	doxygen doxConfig

.PHONY: build doc
