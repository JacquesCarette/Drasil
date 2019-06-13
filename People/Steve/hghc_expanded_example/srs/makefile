ifeq "$(OS)" "Windows_NT"
	# WINDOWS
	RM=del
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S), Linux)
		# LINUX 
		RM=rm
	endif
	ifeq ($(UNAME_S), Darwin)
		# OSX
		RM=rm
	endif
endif

SRS_TEX_NAME=hghc_SRS

.PHONY: all
all: clean srs

.PHONY: srs
srs: $(SRS_TEX_NAME).pdf
	
$(SRS_TEX_NAME).pdf: $(SRS_TEX_NAME).tex
	pdflatex $(SRS_TEX_NAME)
	pdflatex $(SRS_TEX_NAME)

clean:
	@-$(RM) *.aux *.idx *.out *.scn *.toc *.log *.bbl *.blg *.gz *.pdf

