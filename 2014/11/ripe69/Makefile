TARGET=ripe69

all: loss.eps ${TARGET}.pdf

%.pdf: %.tex
	xelatex -interaction=nonstopmode $<

loss.eps: loss.plot loss.dat
	gnuplot loss.plot

loss.dat: loss.py
	python loss.py > loss.dat

clean:
	rm -f ${TARGET}.pdf ${TARGET}.out ${TARGET}.aux ${TARGET}.nav ${TARGET}.snm ${TARGET}.toc
	rm -f ${TARGET}.log
	rm loss.eps loss.dat
	rm -f *~
