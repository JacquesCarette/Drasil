srs: SglPend_SRS.pdf

SglPend_SRS.pdf: SglPend_SRS.tex
	lualatex $(TEXFLAGS) SglPend_SRS
	-bibtex $(BIBTEXFLAGS) SglPend_SRS
	lualatex $(TEXFLAGS) SglPend_SRS
	lualatex $(TEXFLAGS) SglPend_SRS

.PHONY: srs
