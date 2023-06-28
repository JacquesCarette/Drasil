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

# Project Name and Purpose: DblPendulum - To predict the motion of a pendulum.

build: DblPendulum$(TARGET_EXTENSION)

DblPendulum$(TARGET_EXTENSION): InputParameters.hpp Constants.hpp Calculations.hpp OutputFormat.hpp Populate.hpp ODE.hpp Control.cpp InputParameters.cpp Constants.cpp Calculations.cpp OutputFormat.cpp Populate.cpp ODE.cpp
	"$(CXX)" Control.cpp InputParameters.cpp Constants.cpp Calculations.cpp OutputFormat.cpp Populate.cpp ODE.cpp --std=c++11 -o DblPendulum$(TARGET_EXTENSION) -I .

run: build
	./DblPendulum$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig InputParameters.hpp Constants.hpp Calculations.hpp OutputFormat.hpp Populate.hpp ODE.hpp Control.cpp
	doxygen doxConfig

.PHONY: build run doc
