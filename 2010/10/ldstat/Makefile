all: ldstat.pdf ldstat.html ldstat.txt
	ln -s ldstat.html index.html

%.pdf: %.ps
	ps2pdf $< $@
%.ps: %.dvi
	dvips -o $@ $<

%.dvi: %.tex
	latex -interaction=nonstopmode $<
	bibtex `echo $< | sed 's@\.tex$$@@'`
	latex -interaction=nonstopmode $<
	latex -interaction=nonstopmode $<

%.html: %.tex
	hevea $<

%.txt: %.tex
	hevea -text $<

clean:
	rm -f ldstat.pdf ldstat.html ldstat.txt *.aux *.bbl *.blg
