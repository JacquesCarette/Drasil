srs: GlassBR_SRS.pdf

GlassBR_SRS.pdf: GlassBR_SRS.tex
	lualatex $(TEXFLAGS) GlassBR_SRS
	-bibtex $(BIBTEXFLAGS) GlassBR_SRS
	lualatex $(TEXFLAGS) GlassBR_SRS
	lualatex $(TEXFLAGS) GlassBR_SRS

.PHONY: srs
