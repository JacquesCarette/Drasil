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

srs: NoPCM_SRS.pdf

NoPCM_SRS.pdf: NoPCM_SRS.tex
	lualatex $(TEXFLAGS) NoPCM_SRS
	-bibtex $(BIBTEXFLAGS) NoPCM_SRS
	lualatex $(TEXFLAGS) NoPCM_SRS
	lualatex $(TEXFLAGS) NoPCM_SRS

.PHONY: srs
