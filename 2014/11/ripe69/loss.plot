set term eps color
set output 'loss.eps'

set title "ideal loss vs. frequency for 15km of 22AWG copper"
set ylabel "loss (dB)"
set xlabel "frequency (kHz)"
plot "loss.dat" using 1:2 with lines notitle;
