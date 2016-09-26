SWHS_PREF = SWHS_
LSS_PREF = $(empty)
SSP_PREF = SSP_
GLASS_PREF = GlassBR_

TESTS = swhs lss ssp glassbr

all: $(TESTS)
	@echo "DONE"

swhs:
	cabal build swhs
	./dist/build/swhs/swhs
	- diff ./stable/swhs/$(SWHS_PREF)SRS.tex ./SRS/$(SWHS_PREF)SRS.tex > $(SWHS_PREF)log.log
	- diff ./stable/swhs/$(SWHS_PREF)MG.tex ./MG/$(SWHS_PREF)MG.tex >> $(SWHS_PREF)log.log
	- diff ./stable/swhs/$(SWHS_PREF)SRS.css ./Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log
	- diff ./stable/swhs/$(SWHS_PREF)SRS.html ./Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log
	
lss:
	cabal build lss
	./dist/build/lss/lss
	- diff ./stable/lss/$(LSS_PREF)SRS.tex ./SRS/$(LSS_PREF)SRS.tex > $(LSS_PREF)log.log
	- diff ./stable/lss/$(LSS_PREF)MG.tex ./MG/$(LSS_PREF)MG.tex >> $(LSS_PREF)log.log
	- diff ./stable/lss/$(LSS_PREF)SRS.css ./Website/$(LSS_PREF)SRS.css >> $(LSS_PREF)log.log
	- diff ./stable/lss/$(LSS_PREF)SRS.html ./Website/$(LSS_PREF)SRS.html >> $(LSS_PREF)log.log
	
ssp:
	cabal build ssp
	./dist/build/ssp/ssp
	- diff ./stable/ssp/$(SSP_PREF)SRS.tex ./SRS/$(SSP_PREF)SRS.tex > $(SSP_PREF)log.log
	- diff ./stable/ssp/$(SSP_PREF)MG.tex ./MG/$(SSP_PREF)MG.tex >> $(SSP_PREF)log.log
	- diff ./stable/ssp/$(SSP_PREF)SRS.css ./Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log
	- diff ./stable/ssp/$(SSP_PREF)SRS.html ./Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log
	
glassbr:
	cabal build glassbr
	./dist/build/glassbr/glassbr
	- diff ./stable/glassbr/$(GLASS_PREF)SRS.tex ./SRS/$(GLASS_PREF)SRS.tex > $(GLASS_PREF)log.log
	- diff ./stable/glassbr/$(GLASS_PREF)MG.tex ./MG/$(GLASS_PREF)MG.tex >> $(GLASS_PREF)log.log
	- diff ./stable/glassbr/$(GLASS_PREF)SRS.css ./Website/$(GLASS_PREF)SRS.css >> $(GLASS_PREF)log.log
	- diff ./stable/glassbr/$(GLASS_PREF)SRS.html ./Website/$(GLASS_PREF)SRS.html >> $(GLASS_PREF)log.log
	
clean: clean_logs
	rm -r ./SRS ./Website ./MIS ./MG ./Code ./dist
	
clean_logs:
	rm *.log