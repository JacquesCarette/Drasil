build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../datafiles/projectile/Launch.jpg" "src/assets/Launch.jpg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/projectile/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/projectile/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/projectile/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/projectile/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/projectile/refvsref.svg" "src/assets/refvsref.svg"
	-mkdir -p "src/assets" && cp "../../../../datafiles/projectile/SystemContextFigure.png" "src/assets/SystemContextFigure.png"

.PHONY: build server assets
