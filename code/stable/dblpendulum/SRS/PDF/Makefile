srs: DblPendulum_SRS.pdf

DblPendulum_SRS.pdf: DblPendulum_SRS.tex
	lualatex $(TEXFLAGS) DblPendulum_SRS
	-bibtex $(BIBTEXFLAGS) DblPendulum_SRS
	lualatex $(TEXFLAGS) DblPendulum_SRS
	lualatex $(TEXFLAGS) DblPendulum_SRS

.PHONY:
