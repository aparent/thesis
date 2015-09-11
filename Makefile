diagrams-jcc-examples = $(patsubst jcc-examples/%.j, images/%.pdf, $(wildcard jcc-examples/*.j))
diagrams-svg = $(patsubst %.svg, %.pdf, $(wildcard images/*.svg))

diagrams = $(diagrams-jcc-examples) $(diagrams-svg)

.PHONY: thesis

all: thesis

thesis: $(diagrams) data/plot-data
	pdflatex -draftmode thesis.tex
	bibtex thesis.aux
	pdflatex -draftmode thesis.tex
	pdflatex thesis.tex

images/%.svg: jcc-examples/%.j
	jcc $^ -O -d $@

%.pdf: %.svg
	inkscape $^ --export-pdf=$@

data/plot-data: code/kara.hs
	runhaskell code/kara.hs
	touch data/plot-data
