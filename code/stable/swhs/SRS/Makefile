srs: SWHS_SRS.pdf

SWHS_SRS.pdf: SWHS_SRS.tex
	lualatex $(TEXFLAGS) SWHS_SRS
	-bibtex $(BIBTEXFLAGS) SWHS_SRS
	lualatex $(TEXFLAGS) SWHS_SRS
	lualatex $(TEXFLAGS) SWHS_SRS

.PHONY: srs
