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

build: SimpleODE$(TARGET_EXTENSION)

SimpleODE$(TARGET_EXTENSION): SimpleODE.cs
	$(CSC) -out:SimpleODE$(TARGET_EXTENSION) SimpleODE.cs

run: build
	./SimpleODE$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
