srs: Projectile_SRS.pdf

Projectile_SRS.pdf: Projectile_SRS.tex
	lualatex $(TEXFLAGS) Projectile_SRS
	-bibtex $(BIBTEXFLAGS) Projectile_SRS
	lualatex $(TEXFLAGS) Projectile_SRS
	lualatex $(TEXFLAGS) Projectile_SRS

.PHONY: srs
