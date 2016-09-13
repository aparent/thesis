diagrams-jcc-examples = $(patsubst jcc-examples/%.j, images/%.pdf, $(wildcard jcc-examples/*.j))
diagrams-svg = $(patsubst %.svg, %.pdf, $(wildcard images/*.svg))

diagrams = $(diagrams-jcc-examples) $(diagrams-svg)

tex-files = memory.tex arithmetic.tex intro.tex jcc.tex header.tex

.PHONY: uw-ethesis

all: thesis.pdf

clean:
	latexmk -C

thesis.pdf: uw-ethesis.pdf
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -sOutputFile=thesis.pdf uw-ethesis.pdf

uw-ethesis.pdf: uw-ethesis.tex uw-ethesis-frontpgs.tex $(tex-files) $(diagrams) data/plot-data
	latexmk -pdf -pdflatex="pdflatex -shell-escape" uw-ethesis.tex

images/%.svg: jcc-examples/%.j
	jcc $^ -O -d $@

%.pdf: %.svg
	inkscape $^ --export-pdf=$@

data/plot-data: code/kara.hs
	runhaskell code/kara.hs
	touch data/plot-data
