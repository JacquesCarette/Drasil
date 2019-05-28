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

build: SWHS$(TARGET_EXTENSION)

SWHS$(TARGET_EXTENSION): Control.cpp InputParameters.cpp OutputFormat.cpp Calculations.cpp InputFormat.cpp InputParameters.hpp OutputFormat.hpp Calculations.hpp InputFormat.hpp
	"$(CXX)" Control.cpp InputParameters.cpp OutputFormat.cpp Calculations.cpp InputFormat.cpp --std=c++11 -o SWHS$(TARGET_EXTENSION)

run: build
	./SWHS$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
