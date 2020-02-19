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

build: Projectile$(TARGET_EXTENSION)

Projectile$(TARGET_EXTENSION): Projectile.cs
	$(CSC) -out:Projectile$(TARGET_EXTENSION) Projectile.cs

run: build
	./Projectile$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig Projectile.cs
	doxygen doxConfig

.PHONY: build run doc
