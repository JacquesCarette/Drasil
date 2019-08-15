srs: GamePhysics_SRS.pdf

GamePhysics_SRS.pdf: GamePhysics_SRS.tex
	lualatex $(TEXFLAGS) GamePhysics_SRS
	-bibtex $(BIBTEXFLAGS) GamePhysics_SRS
	lualatex $(TEXFLAGS) GamePhysics_SRS
	lualatex $(TEXFLAGS) GamePhysics_SRS

.PHONY: srs
