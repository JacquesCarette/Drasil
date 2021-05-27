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

build: PatternTest$(TARGET_EXTENSION)

PatternTest$(TARGET_EXTENSION): main.swift Observer.swift
	swiftc main.swift Observer.swift -o PatternTest$(TARGET_EXTENSION)

run: build
	./PatternTest$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
