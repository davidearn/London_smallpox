## Fig_Fourier_spectrum.R:
## create Fig: 
## Fourier power spectrum of the normalized weekly smallpox mortality
## time series for London, England, 1664-1930. 
## Before computing the power spectrum the
## time series was detrended and square root transformed. 
## The spectrum was smoothed using a modified 
## Daniell window (weighted moving average)

## set working directoty to the source file location
if (interactive()) 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## clear all:
rm(list=ls())

## load the data files:
## mortality data file
##data <- read.table("../Data/master_file_no_mv.csv", header=TRUE, sep = ",");
data <- read.table("../Data/London_smallpox.csv", header=TRUE, sep = ",");

## define data to work with:
## normalized smallpox data, squre root transformed and detrended
## (cleaned: no missing values, no entry types, no UHP)
index <- which(!is.na(data$smpx.norm))
smpx <- data$sqrt.smpx.norm[index]-data$trend.sqrt.smpx.norm[index];
###index <- which(!is.na(data$smpx.no.mv))
###smpx <- data$smpx.no.mv[index]
## numdate for x-axis
numdate <- data$numdate[index];

#######################################
## 1) define smoothing kernel
#######################################
k <- kernel("modified.daniell", c(3,3))

#######################################
## 2) calculate periodogram
#######################################
## calculate the periodogram using a fast Fourier transform, 
## and smooths the result with a series of modified Daniell smoothers 
smpx.per <- spec.pgram(smpx, taper=0, kernel = k, log="no", plot=FALSE)

## spectrum
spectrum <- smpx.per$spec[length(smpx.per$spec):1]

## calculate period
n <- length(smpx)
w <- c(1:(n/2)/(length(smpx.per$spec)*2)*52.177457)
period <- 1/w

#######################################
## 3) PLOT
#######################################

## new plotting window
#quartz(width=10, height=4)

## to save figure in pdf
#if (!interactive()) 
  pdf("../Figures_and_Tables/Fig_Fourier_spectrum.pdf", width = 10, height = 6, family = "ArialMT", paper = "special")

par(mar=c(3,3,1,1)) #mar=c(bottom, left, top, right)

## set up plot area
xmax <- 8
xx <- period[1:length(smpx.per$spec)]
yy <- smpx.per$spec
ymax <- max(yy[xx<xmax],na.rm=TRUE)
##yy <- yy / ymax # normalize to max 1
plot(xx, yy, 
     type="l", las=1, xaxs="i", yaxs="i", bty="L",
     lwd=3,
     xlim=c(0,xmax), 
     ###ylim=c(0,0.68),
     ylim=c(0,ymax*1.06),
     ## lab=c(15, 5, 7), 
     lab=c(9, 6, 7), 
     main="", 
     xlab="Period [years]", ylab="Spectral density", 
     cex.lab=1.2, cex.axis=1.1, tck=-0.01, mgp=c(2, 0.5,0))

#######################################
## 4) Add peaks
#######################################

## Identify peaks using
## visual intervals
k <- c(35,48,79,109,118,199,6200)

## find maximum in each visual interval
period.peaks <- sapply(1:6, 
                  function(i){
                    peak <- which(smpx.per$spec==max(smpx.per$spec[k[i+1]:k[i]]))
                    ##abline (v = period[peak], lty="dotted")
                    return(peak)
                  });

## identify main periods
mainPeriodStr <- period[period.peaks]

## add main period points to the plot
## including label
points(period[period.peaks], smpx.per$spec[period.peaks], 
              type="p", pch=21,bg="red", col="dark red")
label <- paste(round(mainPeriodStr, 1))
##text(period[period.peaks]+0.06, smpx.per$spec[period.peaks], label, cex=0.8, adj=0)
text(period[period.peaks], smpx.per$spec[period.peaks]+0.02, label, cex=0.95)

if (!interactive()) 
  dev.off()
