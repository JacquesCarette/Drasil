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

srs: Chipmunk_SRS.pdf

Chipmunk_SRS.pdf: Chipmunk_SRS.tex
	lualatex $(TEXFLAGS) Chipmunk_SRS
	-bibtex $(BIBTEXFLAGS) Chipmunk_SRS
	lualatex $(TEXFLAGS) Chipmunk_SRS
	lualatex $(TEXFLAGS) Chipmunk_SRS

.PHONY: srs
