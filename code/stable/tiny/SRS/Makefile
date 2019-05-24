srs: Tiny_SRS.pdf

Tiny_SRS.pdf: Tiny_SRS.tex
	lualatex $(TEXFLAGS) Tiny_SRS
	-bibtex $(BIBTEXFLAGS) Tiny_SRS
	lualatex $(TEXFLAGS) Tiny_SRS
	lualatex $(TEXFLAGS) Tiny_SRS

.PHONY: srs
