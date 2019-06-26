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

SWHS$(TARGET_EXTENSION): InputParameters.hpp InputFormat.hpp OutputFormat.hpp Control.cpp InputParameters.cpp InputFormat.cpp OutputFormat.cpp
	"$(CXX)" Control.cpp InputParameters.cpp InputFormat.cpp OutputFormat.cpp --std=c++11 -o SWHS$(TARGET_EXTENSION)

run: build
	./SWHS$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
