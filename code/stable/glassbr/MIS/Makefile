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

mis: GlassBR_MIS.pdf

GlassBR_MIS.pdf: GlassBR_MIS.tex
	lualatex $(TEXFLAGS) GlassBR_MIS
	-bibtex $(BIBTEXFLAGS) GlassBR_MIS
	lualatex $(TEXFLAGS) GlassBR_MIS
	lualatex $(TEXFLAGS) GlassBR_MIS

.PHONY: mis
