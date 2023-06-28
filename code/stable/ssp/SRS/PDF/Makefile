srs: SSP_SRS.pdf

SSP_SRS.pdf: SSP_SRS.tex
	lualatex $(TEXFLAGS) SSP_SRS
	-bibtex $(BIBTEXFLAGS) SSP_SRS
	lualatex $(TEXFLAGS) SSP_SRS
	lualatex $(TEXFLAGS) SSP_SRS

.PHONY:
