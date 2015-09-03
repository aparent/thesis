diagrams-jcc-examples = $(patsubst jcc-examples/%.j, images/%.pdf, $(wildcard jcc-examples/*.j))
diagrams-svg = $(patsubst %.svg, %.pdf, $(wildcard images/*.svg))

diagrams = $(diagrams-jcc-examples) $(diagrams-svg)

.PHONY: thesis

all: thesis

thesis: $(diagrams) 
	pdflatex thesis.tex
	bibtex thesis.aux
	pdflatex thesis.tex
	pdflatex thesis.tex

images/%.svg: jcc-examples/%.j
	jcc $^ -O -d $@


%.pdf: %.svg
	inkscape $^ --export-pdf=$@
