srs: PDController_SRS.pdf

PDController_SRS.pdf: PDController_SRS.tex
	lualatex $(TEXFLAGS) PDController_SRS
	-bibtex $(BIBTEXFLAGS) PDController_SRS
	lualatex $(TEXFLAGS) PDController_SRS
	lualatex $(TEXFLAGS) PDController_SRS

.PHONY:
