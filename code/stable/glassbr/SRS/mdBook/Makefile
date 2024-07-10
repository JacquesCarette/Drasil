build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/glassbr/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/glassbr/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/glassbr/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/glassbr/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/glassbr/refvsref.svg" "src/assets/refvsref.svg"
	-mkdir -p "src/assets" && cp "../../../../datafiles/glassbr/ASTM_F2248-09.png" "src/assets/ASTM_F2248-09.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/glassbr/ASTM_F2248-09_BeasonEtAl.png" "src/assets/ASTM_F2248-09_BeasonEtAl.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/glassbr/physicalsystimage.png" "src/assets/physicalsystimage.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/glassbr/SystemContextFigure.png" "src/assets/SystemContextFigure.png"

.PHONY: build server assets
