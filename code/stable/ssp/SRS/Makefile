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

srs: SSP_SRS.pdf

SSP_SRS.pdf: SSP_SRS.tex
	lualatex $(TEXFLAGS) SSP_SRS
	-bibtex $(BIBTEXFLAGS) SSP_SRS
	lualatex $(TEXFLAGS) SSP_SRS
	lualatex $(TEXFLAGS) SSP_SRS

.PHONY: srs
