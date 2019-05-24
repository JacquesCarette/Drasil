ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
    RM=del
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
        RM=rm
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
        RM=rm
    endif
endif

srs: Projectile_SRS.pdf

Projectile_SRS.pdf: Projectile_SRS.tex
	lualatex $(TEXFLAGS) Projectile_SRS
	-bibtex $(BIBTEXFLAGS) Projectile_SRS
	lualatex $(TEXFLAGS) Projectile_SRS
	lualatex $(TEXFLAGS) Projectile_SRS

.PHONY: srs
