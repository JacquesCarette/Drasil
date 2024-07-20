srs: DblPend_SRS.pdf

DblPend_SRS.pdf: DblPend_SRS.tex
	lualatex $(TEXFLAGS) DblPend_SRS
	-bibtex $(BIBTEXFLAGS) DblPend_SRS
	lualatex $(TEXFLAGS) DblPend_SRS
	lualatex $(TEXFLAGS) DblPend_SRS

.PHONY: srs
