srs: NoPCM_SRS.pdf

NoPCM_SRS.pdf: NoPCM_SRS.tex
	lualatex $(TEXFLAGS) NoPCM_SRS
	-bibtex $(BIBTEXFLAGS) NoPCM_SRS
	lualatex $(TEXFLAGS) NoPCM_SRS
	lualatex $(TEXFLAGS) NoPCM_SRS

.PHONY:
