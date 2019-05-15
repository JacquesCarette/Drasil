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

srs: SWHS_SRS.pdf

SWHS_SRS.pdf: SWHS_SRS.tex
	lualatex $(TEXFLAGS) SWHS_SRS
	-bibtex $(BIBTEXFLAGS) SWHS_SRS
	lualatex $(TEXFLAGS) SWHS_SRS
	lualatex $(TEXFLAGS) SWHS_SRS

.PHONY: srs
