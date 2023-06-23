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

# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build: Projectile$(TARGET_EXTENSION)

Projectile$(TARGET_EXTENSION): main.swift
	swiftc main.swift -o Projectile$(TARGET_EXTENSION)

run: build
	./Projectile$(TARGET_EXTENSION) $(RUNARGS)

doc: main.swift

.PHONY: build run doc
