build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/pdcontroller/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/pdcontroller/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/pdcontroller/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/pdcontroller/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/pdcontroller/refvsref.svg" "src/assets/refvsref.svg"
	-mkdir -p "src/assets" && cp "../../../../datafiles/pdcontroller/Fig_PDController.png" "src/assets/Fig_PDController.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/pdcontroller/Fig_SystemContext.png" "src/assets/Fig_SystemContext.png"

.PHONY: build server assets
