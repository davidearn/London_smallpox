## 30 Jun 2020
## Fig_spectral_peaks.R:
## create Fig: 
## Primary spectral peaks in time series of smallpox mortality in London,
## 1664–1930, as estimated in previous work [16, 41] (based on traditional spectral analysis
## of annual data), and in this paper (based on a wavelet analysis of weekly data). For the
## wavelet analysis, all spectral peaks above a threshold are shown

## set working directory to the source file location
if (interactive()) 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## clear all:
rm(list=ls())

source("functions_for_plots.R") # transparent_colour

## read the data files containing spectral peaks:
read_peaks <- function(fn,...) {
    read.table(fn, header=FALSE, col.names=c("year", "period"), ...)
}
us <- read_peaks("../Data/Wavelet_peaks.csv", sep=" ") # created by Wavelet_period.R
Duncan <- read_peaks("../Data/Spectral_peaks_Duncan.csv", sep=",")
Cliff <- read_peaks("../Data/Spectral_peaks_Cliff.csv", sep=",")
peaks.by.eye <- read.csv("../Data/peaks_by_eye.csv")
library(dplyr)
mawin <- 9 # moving average window
pbe <- (peaks.by.eye
    %>% mutate(diff = c(NA,diff(numdate)))
    %>% mutate(diff.avg = stats::filter(diff, rep(1/mawin,mawin)))
)
## re-order so we can connect the dots in the wavelet peaks
top1 <- (us
    %>% filter( year < 1720  & period > 4 )
)
##top2 <- (us  # now joined with above
##    %>% filter( year > 1680 & year < 1720 & period > 4 )
##)
top3 <- (us
    %>% filter( year > 1720 & year < 1920 & period > 5 )
)
top4 <- (us
    %>% filter( (year > 1763 & year < 1800 & period > 2.7) )
)
bottom1 <- (us
    %>% filter( (year < 1690 & period <= 3)
               | (year > 1690 & year < 1740 & period < 2.8)
               | (year > 1740 & year < 1765)
               | (year > 1765 & year < 1820 & period >=2 & period < 2.8)
               | (year > 1820 & year < 1837)
               )
)
## bottom2 <- (us
##    %>% filter(period < 1.5)  # two annual points
##)
bottom3 <- (us
    %>% filter( (year > 1820 & year < 1920 & period < 4.5) )
)
mid1 <- (us
    %>% filter( (year < 1745 & period > 2.8 & period < 4) )
)
mid2 <- (us
    %>% filter( (year > 1860 & year < 1920 & period > 4.2 & period < 5.45) )
    %>% arrange( period )
)
## clear from wavelet, easiest to just write down:
extra <- data.frame(year=c(1787,1790,NA,1806,1806),
                    period=c(2.8,2.2,NA,1.75,2.4) )
## annual power by eye, but mostly below Olga's threshold:
annual <- data.frame( year=c(1665,1815), period=c(1,1) )
## for under dot on legend:
legend.line <- data.frame( year=c(1668.5,1674), period=c(7.85,7.85) )

branch.list <- list(top1, top3, top4, bottom1, bottom3, mid1, mid2,
                    extra, annual, legend.line)
branch.col <- transparent_colour("pink")
branch.lwd <- 10

## remove points outside cone of influence
us <- subset(us, year < 1920)

if (!interactive()) {
    plos.width <- 13.2 / 2.54 # 13.2 cm / 2.54 cm/inch
    pdf("../Figures_and_Tables/Fig_spectral_peaks.pdf", family = "ArialMT", height=plos.width*1, width=plos.width)
    ##tiff("../images/Fig_spectral_peaks.tiff")
    ##library(tikzDevice)
    ##tikz("../images/Fig_spectral_peaks.tex", height=3, width=6, standAlone=TRUE)
}

## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
mar.orig <- par("mar")
par(mar = c(2,3,0.5,0.5))

## colours
mycols <- c(us="darkred", Duncan="orange", Cliff="lightblue",
            peak.to.peak="white")
            ##peak.to.peak="darkgrey")
##jc <- matlab::jet.colors(200)
##mycols <- c(us=jc[200], Duncan=jc[160], Cliff=jc[65])

## PANEL 1

par(mfrow=c(2,1))
## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]

## set up plot
plot(data = us, period ~ year, type = "n", las=1,
     lab = c(13,8,13), xaxs="i", yaxs="i",
     xlim = c(1664,1930),
     ylim = c(0,8.5),
     ylab = ""
)
title(ylab = "Period  [years]", line = 2)
abline(v = seq(1680,1920,by=20), h = 1:8, col="lightgrey", lty="dotted")
lines(data=Cliff, period ~ year, col=mycols["Cliff"], lwd=6)
lines(data=Duncan, period ~ year, col=mycols["Duncan"], lwd=4)
for (dd in branch.list) lines(period ~ year, data=dd, col=branch.col, lwd=branch.lwd)
points(data=us, period ~ year, pch=21, bg=mycols["us"], col=mycols["us"], cex=0.5)

legend("topleft", bty="n", cex=0.8, pt.cex=c(0.5,0.5,0.8,1),
       legend = c("Wavelet (Fig 4B)", "Duncan et al [20, Table 1]", "Cliff et al [47, Fig 2.15]"),
       pch=21,
       pt.bg=mycols,
       ##col=c("black", mycols["Duncan"], mycols["Cliff"])
       col=c(mycols[1:3],"grey")
       )

## PANEL 2

## set up plot
plot(data = pbe, diff ~ numdate, type = "n", las=1,
     lab = c(13,8,13), xaxs="i", yaxs="i",
     xlim = c(1664,1930),
     ylim = c(0,8.5),
     ylab = ""
)
title(ylab = "Time  [years]", line = 2)

abline(v = seq(1680,1920,by=20), h = 1:12, col="lightgrey", lty="dotted")

points( diff ~ numdate, data=pbe, pch=21, bg=mycols["peak.to.peak"], col="grey", cex=1, xpd=NA, type="o" )
points( diff.avg ~ numdate, data=pbe, pch=21, bg="black", col="grey", cex=0.7, type="o" )

legend("topleft", bty="n", cex=0.8, pt.cex=c(1,0.7),
       legend = c("Peak to peak intervals", "9 point moving average"),
       pch=21,
       pt.bg=c("white","black"),
       col=c("grey")
       )

if (!interactive()) 
  dev.off()
