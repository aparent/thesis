DIA = $(patsubst jcc-examples/%.j, images/%.pdf, $(wildcard jcc-examples/*.j))

.PHONY: thesis

thesis: $(DIA) 
	pdflatex --shell-escape thesis.tex
	bibtex thesis.aux
	pdflatex --shell-escape thesis.tex
	pdflatex --shell-escape thesis.tex

images/%.svg: jcc-examples/%.j
	jcc $^ -O -d $@


%.pdf: %.svg
	inkscape $^ --export-pdf=$@
