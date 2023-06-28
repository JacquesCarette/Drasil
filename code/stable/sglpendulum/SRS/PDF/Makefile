srs: SglPendulum_SRS.pdf

SglPendulum_SRS.pdf: SglPendulum_SRS.tex
	lualatex $(TEXFLAGS) SglPendulum_SRS
	-bibtex $(BIBTEXFLAGS) SglPendulum_SRS
	lualatex $(TEXFLAGS) SglPendulum_SRS
	lualatex $(TEXFLAGS) SglPendulum_SRS

.PHONY:
