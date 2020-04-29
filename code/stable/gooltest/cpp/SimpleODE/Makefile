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

build: SimpleODE$(TARGET_EXTENSION)

SimpleODE$(TARGET_EXTENSION): SimpleODE.cpp T_ODE.hpp T_ODE.cpp
	"$(CXX)" T_ODE.cpp SimpleODE.cpp --std=c++11 -o SimpleODE$(TARGET_EXTENSION)

run: build
	./SimpleODE$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
