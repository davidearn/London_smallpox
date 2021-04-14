# London_smallpox

Data and R scripts associated with

    "Patterns of smallpox mortality in London, England, over three centuries"
	by Olga Krylova and David J. D. Earn.
    PLoS Biology, 2020

## DOI of the manuscript
10.1371/journal.pbio.3000506

## Article Title
Patterns of smallpox mortality in London, England, over three centuries.

# Contents
## Data/London_smallpox.csv
The original digitized weekly reported smallpox deaths (column `smpx`), birth (column `births`) and all-cause mortality (column `acm`) records for London throughout the period 1661-1934. We also included various transformations of this data such as trends, normalizations, etc., that were used for the main analysis.

## Data/LondonPopulation1550_1931annual.csv
Estimated annual population of London for 1550-1931.

## Data/peaks_by_eye.csv
Epidemic peaks, i.e., the highest value of normalized smallpox mortality during the epidemic year, which were identified by visual inspection.

## Data/Wavelet_peaks.csv
The primary spectral peaks, as estimated in this paper (based on a wavelet analysis of weekly data)

## Data/Spectral_peaks_Cliff.csv
The primary spectral peaks, as estimated in previous work by Cliff and
co-workers in "Infectious diseases: Emergence and re-emergence: a
geographical analysis." Oxford University Press (2009).

## Data/Spectral_peaks_Duncan.csv
The primary spectral peaks, as estimated in previous work by Duncan
and co-workers in "Oscillatory dynamics of smallpox and the impact of
vaccination." _Journal of Theoretical Biology_ **183**(4):447-454 (1996).

## Data/annual_data.csv
Annual smallpox deaths, all-cause mortality, and births based on the data presented in this paper (`Data/London_smallpox.csv`, variable `smpx`). 

## Data/Creighton_annual_smallpox.csv
Annual smallpox deaths and all-couse mortality from Creighton (1894).

## Data/WaveletDiagram folder
The data files that are used to plot the wavelet spectrum. 

## R_code folder
R scripts that create all the figures in the text of the manuscript. The R code uses data in the `Data` folder as input, and outputs Figures and Tables into the `Figures_and_Tables` folder.

## Figures_and_Tables folder
Output that is produced by running R scripts in the `R_code` folder, i.e. all figures and tables provided in the paper.
