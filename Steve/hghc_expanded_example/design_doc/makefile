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

MG_TEX_NAME=hghc_MG
MIS_TEX_NAME=hghc_MIS

.PHONY: all
all: clean mg mis

.PHONY: mg
mg: $(MG_TEX_NAME).pdf
	
$(MG_TEX_NAME).pdf: $(MG_TEX_NAME).tex
	pdflatex $(MG_TEX_NAME)
	pdflatex $(MG_TEX_NAME)
    
.PHONY: mis
mis: $(MIS_TEX_NAME).pdf
    
$(MIS_TEX_NAME).pdf: $(MIS_TEX_NAME).tex
	pdflatex $(MIS_TEX_NAME)
	pdflatex $(MIS_TEX_NAME)    
    
clean:
	@-$(RM) *.aux *.idx *.out *.scn *.toc *.log *.bbl *.blg *.gz *.pdf *.bak *.synctex

