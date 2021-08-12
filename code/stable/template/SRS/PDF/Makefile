srs: Template_SRS.pdf

Template_SRS.pdf: Template_SRS.tex
	lualatex $(TEXFLAGS) Template_SRS
	-bibtex $(BIBTEXFLAGS) Template_SRS
	lualatex $(TEXFLAGS) Template_SRS
	lualatex $(TEXFLAGS) Template_SRS

.PHONY: srs
