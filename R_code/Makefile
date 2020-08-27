
d = ../Figures_and_Tables

all: Fig_Fourier_spectrum.pdf Fig_annual_smpx.pdf Fig_spectral_peaks.pdf \
     Fig_acm.pdf Fig_seasonality.pdf Fig_timeplot.pdf

%.pdf: %.R functions_for_plots.R
	time R CMD BATCH --vanilla $<

clean:
	rm -f *.Rout

fresh: clean
	rm -f $d/*.pdf
