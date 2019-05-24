srs: Chipmunk_SRS.pdf

Chipmunk_SRS.pdf: Chipmunk_SRS.tex
	lualatex $(TEXFLAGS) Chipmunk_SRS
	-bibtex $(BIBTEXFLAGS) Chipmunk_SRS
	lualatex $(TEXFLAGS) Chipmunk_SRS
	lualatex $(TEXFLAGS) Chipmunk_SRS

.PHONY: srs
