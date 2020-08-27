
The data files in this folder are used to plot the wavelet spectrum. 

## power.csv
Power wavelet spectrum.

## period.csv
The vector of "Fourier" periods (in time units). 

## pvalue.csv
Defines pvalue (=0.05).

## pvp.csv
Matrix of the Pvalue of the power wavelet. 95\% confidence contours, estimated from 1000 bootstrapped time series. It's used to create contour of the area with higher power.

## rid.csv
Ridge values approximated by the maximum of the power wavelet matrix (these maxima are named RIDGE in the context of Wavelet Analysis). Theses data is used to create contour of the most prominent period, i.e. local maxima of wavelet power. 

## coi.csv
The "cone-of-influence", which is a vector of points that contains the limit of the region where the wavelet transform is influenced by edge effects.


