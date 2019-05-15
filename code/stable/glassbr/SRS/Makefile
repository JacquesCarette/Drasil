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

srs: GlassBR_SRS.pdf

GlassBR_SRS.pdf: GlassBR_SRS.tex
	lualatex $(TEXFLAGS) GlassBR_SRS
	-bibtex $(BIBTEXFLAGS) GlassBR_SRS
	lualatex $(TEXFLAGS) GlassBR_SRS
	lualatex $(TEXFLAGS) GlassBR_SRS

.PHONY: srs
