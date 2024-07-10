build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../datafiles/ssp/ForceDiagram.png" "src/assets/ForceDiagram.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/ssp/IndexConvention.png" "src/assets/IndexConvention.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/ssp/PhysSyst.png" "src/assets/PhysSyst.png"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/ssp/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/ssp/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/ssp/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/ssp/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/ssp/refvsref.svg" "src/assets/refvsref.svg"
	-mkdir -p "src/assets" && cp "../../../../datafiles/ssp/SystemContextFigure.png" "src/assets/SystemContextFigure.png"

.PHONY: build server assets
