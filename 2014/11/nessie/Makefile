all: build docs

build:
	ghc --make MasterEx.lhs

docs: nessie.pdf

clean:
	find . -name \*~ -exec rm -f '{}' ';'
	rm -f MasterEx *.o *.hi
	rm -f *.aux *.log *.out

nessie.pdf: nessie.tex literature.bib Master.lhs MasterEx.lhs
	xelatex -interaction=nonstopmode nessie.tex
	bibtex nessie
	xelatex -interaction=nonstopmode nessie.tex
	xelatex -interaction=nonstopmode nessie.tex
