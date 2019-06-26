ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
    endif
endif

build: GlassBR$(TARGET_EXTENSION)

GlassBR$(TARGET_EXTENSION): InputParameters.hpp DerivedValues.hpp InputConstraints.hpp InputFormat.hpp OutputFormat.hpp Calculations.hpp ReadTable.hpp Interpolation.hpp Control.cpp InputParameters.cpp DerivedValues.cpp InputConstraints.cpp InputFormat.cpp OutputFormat.cpp Calculations.cpp ReadTable.cpp Interpolation.cpp
	"$(CXX)" Control.cpp InputParameters.cpp DerivedValues.cpp InputConstraints.cpp InputFormat.cpp OutputFormat.cpp Calculations.cpp ReadTable.cpp Interpolation.cpp --std=c++11 -o GlassBR$(TARGET_EXTENSION)

run: build
	./GlassBR$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
