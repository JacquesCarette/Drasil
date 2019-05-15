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

srs: Tiny_SRS.pdf

Tiny_SRS.pdf: Tiny_SRS.tex
	lualatex $(TEXFLAGS) Tiny_SRS
	-bibtex $(BIBTEXFLAGS) Tiny_SRS
	lualatex $(TEXFLAGS) Tiny_SRS
	lualatex $(TEXFLAGS) Tiny_SRS

.PHONY: srs
