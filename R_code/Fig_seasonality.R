## Fig_seasonality.R:
## create Fig: 
## Seasonal variations of smallpox mortality in 
## London, England, 1664-1930.
## The normalized smallpox time series was
## detrended and square root-transformed before constructing 
## the heat map. Dark red dots indicate the week of each year 
## with the highest value of normalized smallpox mortality. 
## Zeros are not necessarily displayed with dark blue, 
## because the detrending shifts the end of the time series up

## set working directoty to the source file location
if (interactive()) 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## clear all:
rm(list=ls())

show.wavelet.image <- TRUE
show.panel.labels <- TRUE
cex.panel.label <- 3
position.panel.label <- "right"

## load functions: 
source("functions_for_plots.R")

## load packages:
library(matlab)
library(graphics)
library(circular)

## define data to work with:

## load the data files:
## mortality data file
##data <- read.table("../Data/master_file_no_mv.csv", header=TRUE, sep = ",");
data<-read.csv("../Data/London_smallpox.csv", header=TRUE, sep = ",", comment.char="#")

## normalized smallpox data
## (cleaned: no missing values, no entry typo, no UHP)
## There are NA calues at the beginning of the data series
## as smallpox data starts from 1664 while the acm data start from 1661
index <- which(!is.na(data$smpx.norm))
smpx <- data$smpx.norm[index];
## numdate for x-axis
numdate <- data$numdate[index];
time <- numdate;

## B)
## data output from the MATLAB wavelet code
#
##wavelet.folder <- "../Data/WaveletDiagram/Saved_data/"
wavelet.folder <- "../Data/WaveletDiagram/"
period <- read.table(paste0(wavelet.folder,"period.csv"), header=FALSE, sep = ",");
period <- as.matrix(period);
#
power <- read.table(paste0(wavelet.folder,"power.csv"), header=FALSE, sep = ",");
power <- as.matrix(power);
#
##ipp <- read.table(paste0(wavelet.folder,"ipp.csv"), header=FALSE, sep = ",");
##ipp <- as.matrix(ipp);
#
pvp<-read.table(paste0(wavelet.folder,"pvp.csv"), header=FALSE, sep = ",")
pvp<-as.matrix(pvp)
#
pvalue <- read.table(paste0(wavelet.folder,"pvalue.csv"), header=FALSE, sep = ",");
pvalue <- as.matrix(pvalue);
#
rid <- read.table(paste0(wavelet.folder,"rid.csv"), header=FALSE, sep = ",");
rid <- as.matrix(rid);
#
coi <- read.table(paste0(wavelet.folder,"coi.csv"), header=FALSE, sep = ","); 
coi <- as.matrix(coi);


###################
## PLOT
###################
## new plotting window
#quartz(width=10, height=11.4)

## to save figure in pdf
#if (!interactive())
 pdf("../Figures_and_Tables/Fig_seasonality.pdf", 
     onefile=TRUE, family = "ArialMT", width = 11, height = 15, paper = "special")


##
layout(matrix(c(1,2,3,4,5),5,1), height=c(3.6,3.4,3.4,3.6,3))
par(mar=c(0,0,0,1));

n <- max(data$smpx, na.rm=TRUE);
m <- max(smpx, na.rm=TRUE);

#######################################
## 1) set up plot area
#######################################
setup_plot(numdate, smpx, 
           y.ticks=c(0, 0.1, 0.2, 0.3, 0.4),
           ylim.vector = c(0, 0.5),
           y.text = 0.23,
           axis.pos = c(-0.014, 1663),
           cex.axis = 1.1, 
           text.here = "Normalized smallpox mortality",
           y.colour="black")

######################################
## 2) plot normlized smallpox time series
######################################
## normalized smallpox
lines(numdate, smpx, lwd=0.55, lty=1, col="black");

#############################################
## 3) insert intervention uptake levels
##    as coloured bars: 
##    yellow green-dark olive for variolation 
##    with yellow green indicating the lowest level and dark olive the highest; 
##    and yellow-red for vaccination with light yellow indicating the lowest level 
##    and red the highest level.
#############################################
## m - reference y-coordinate

text(1650, 1.128*m, "Intervention
uptake 
level", cex=1.1, srt=0, col="black", font=2);

## insert vertical lines to connect timeline 
## with intervention uptake bars 
intervention_years <- c(1721, 1728, 1740, 1768, 1790, 1796, 1808, 1840)
## assignment to dummy_1 avoids irrelevant output of the sapply
dummy_1 <- sapply(1:length(intervention_years), 
                  function(i){
                    segments(intervention_years[i],-0.035*m, 
                             intervention_years[i], 1.213*m, 
                             col="black", lty=3)
                  });

## VARIOLATION
## insert variolation bar
variolation_bar(smpx, col_bar=green_pallet(), 
                magnification_par=3.4, cex.coeff=1.6);

## VACCINATION
## insert vaccination bar
vaccination_bar(smpx, col_bar=heat.colors(40)[30:1], 
                magnification_par=3.4, cex.coeff=1.5);

######################################
## 4) add epidemic peaks data
######################################
## read data file where epidemic peaks were identified 
peaks <- read.csv("../Data/peaks_by_eye.csv")

## match by numdate with original data
peaks.index <- match(peaks$numdate, data$numdate)

## plot peaks as red dots
points(data$numdate[peaks.index],data$smpx.norm[peaks.index], col=jet.colors(200)[185], pch=20)

## panel label
if (show.panel.labels) legend(position.panel.label, legend=" A", bty="n", cex=cex.panel.label)

#######################################
## 4) Wavelet: set up plot area
#######################################
par(mar=c(0,0,1.8,1))
ylab <- c(1,2,3,4,5,6,8); ## vector of y-axis labels
setup_plot(numdate, smpx, 
           y.ticks=log(ylab, base=2),
           y.labels = ylab,
           ylim.vector=c(log(max(period), base=2), log(min(period), base=2)),
           y.text = 1.1,
           axis.pos = c(log(max(period), base=2), numdate[1]),
           cex.axis = 1.1, 
           text.here = "Period  [years]",
           y.colour="black")

## WAVELET SPECTRUM
## add wavelet diagram
par(xaxt="n", yaxt="n", bty="n")
if (show.wavelet.image) {
imagesc(x=time, y=t(log(period, base=2)), 
        sqrt(power[seq(130,1),])/max(sqrt(power)), 
        col=jet.colors(50), add=TRUE, bty="n"
        ## , useRaster = TRUE ## works only with a regular grid
        )

## CONTOUR
## add contour to identify area of higher power
contour(x=time, y=t(log(period, base=2)), 
        z=t(pvp), levels=c(pvalue/100, pvalue/100), 
        add=TRUE, drawlabels=FALSE, lwd=0.3, 
        ylim=c(log(max(period), base=2), log(min(period), base=2)))

## CONTOUR
## add countour of the most promenent period
contour(x=time, y=t(log(period, base=2)),
        z=t(rid), levels=c(1,1), col="black", lwd=1.15,
        add=TRUE, drawlabels=FALSE)

## CONE OF INFLUENCE (COI) 
## Below the \cone of influence" the calculation of wavelet
## power is less accurate because it includes edges of the
## time series that have been zero-padded to make the length of the series a power of 2
## add coi
lines(time[which(log(coi, base=2)<=3.3&log(coi, base=2)>=-1)], 
      log(coi, base=2)[which(log(coi, base=2)<=3.3&log(coi, base=2)>=-1)], lwd=1)

## insert vertical lines to connect timeline 
## with intervention uptake bars 
intervention_years <- c(1721, 1728, 1740, 1768, 1790, 1796, 1808, 1840)
## assignment to dummy_1 avoids irrelevant output of the sapply
dummy_1 <- sapply(1:length(intervention_years), 
                  function(i){
                    segments(intervention_years[i],log(max(period), base=2), 
                             intervention_years[i], log(min(period), base=2), 
                             col="black", lty=3)
                  });

#######################################
## add horizontal lines to visually seperate each period
#######################################
segments(1664, log(1,base = 2), 1930, log(1,base = 2), lty=3, lwd=.5)
segments(1664, log(2,base = 2), 1930, log(2,base = 2), lty=3, lwd=.5)
segments(1664, log(3,base = 2), 1930, log(3,base = 2), lty=3, lwd=.5)
segments(1664, log(4,base = 2), 1930, log(4,base = 2), lty=3, lwd=.5)
segments(1664, log(5,base = 2), 1930, log(5,base = 2), lty=3, lwd=.5)
segments(1664, log(6,base = 2), 1930, log(6,base = 2), lty=3, lwd=.5)
segments(1664, log(7,base = 2), 1930, log(7,base = 2), lty=3, lwd=.5)
segments(1664, log(8,base = 2), 1930, log(8,base = 2), lty=3, lwd=.5)

} # endif show.wavelet.image

## panel label
if (show.panel.labels) legend(position.panel.label, legend=" B", bty="n", cex=cex.panel.label)

######################################
## 5) SEASONALITY PLOT
######################################
## STEP 1:
## creat seasonality matrix data to plot so that
## each row represents one year of data

k <- 1 ## index for all data values
j <- 1 ## index for the year
seasonality.data <- matrix(NA, 269, 53) ## (, number of years, number of weeks)  

while (j <= 269)
{
  if (k==length(data$week)) {stop}
  seasonality.data[j, data$week[k]] <- data$detr.sqrt.smpx.norm[k]
  while ((data$week[k+1]-data$week[k])==1)
  {
    m <- data$week[k+1]
    seasonality.data[j, m] <- data$detr.sqrt.smpx.norm[k+1]
    k <- k+1
  }
  k <- k+1
  j <- j+1
}

## STEP 2:
## find year and a week of the epidemic peaks 
peak.year <- floor(data$numdate[peaks.index]) ## year
p <- data$week[peaks.index] ## week
p[which(p==53)] <- 52 ## if the week is 53 change to 52 as we plot only week 1:52

#######################################
## set up plot area
#######################################
par(mar=c(0,0,1.8,1))
par(xaxt="l", yaxt="l")
y.ticks <- c(0, 13, 26, 39, 52); ## vector of y-axis labels
setup_plot(seq(1664,1931), seq(1,52, length=268), 
           y.ticks=y.ticks,
           #y.labels = c("Winter","Spring","Summer","Autumn",""),
           y.labels = c("Winter","Spring","Summer","Fall", "Winter"),
           ylim.vector=c(0.5, 52.5),
           y.text = 26,
           y.las = 0,
           axis.pos = c(0, 1663),
           cex.axis = 1.1, 
           text.here = "Seasonal pattern",
           y.colour="black" ,
           h.adj = 0.5
)

#######################################
## add seasonality image
#######################################
image(x=seq(1661,1930), y=seq(1,52), (seasonality.data[,seq(1,52)]), 
      ylim=c(1,52), col=jet.colors(200), xlim=c(1664,1931), add=TRUE);

#######################################
## add horizontal lines to visually seperate each season
#######################################
segments(1663, 13, 1930, 13, lty=2, col="darkgrey", lwd=1)
segments(1663, 26, 1930, 26, lty=2, col="darkgrey", lwd=1)
segments(1663, 39, 1930, 39, lty=2, col="darkgrey", lwd=1)

## legend
cols <- jet.colors(200)
## points(x=1910, y=40, pch=22, bg=cols[1], cex=2)
##ss <- seasonality.data
##mylegend <- c(min(ss,na.rm=TRUE), median(ss,na.rm=TRUE), max(ss,na.rm=TRUE))
##mylegend <- rev(signif(mylegend,3))
mylegend <- seq(1,-1,length=11)
mycols <- cols[trunc(seq(200,1,length=11))]
legend(x=1915, y=26, yjust=0.5, pt.cex=1.5, pch=22,
       legend = mylegend,
       pt.bg = mycols
)

## panel label
if (show.panel.labels) legend(position.panel.label, legend=" C", bty="n", cex=cex.panel.label)

#############################################
## 6) insert peaks pannel
#############################################
par(mar=c(1.5,0,2,1))
y.ticks <- c(0, 13, 26, 39, 52); ## vector of y-axis labels
setup_plot(seq(1664,1931), seq(1,52, length=268), 
           y.ticks=y.ticks,
           #y.labels = c("Winter","Spring","Summer","Autumn",""),
           y.labels = c("Winter","Spring","Summer","Fall", "Winter"),
           ylim.vector=c(0.5, 52.5),
           y.text = 26,
           y.las = 0,
           axis.pos = c(0, 1663),
           cex.axis = 1.1, 
           text.here = "Smallpox mortality peaks",
           y.colour="black" ,
           h.adj = 0.5
)

points(peak.year+0.5, p, pch=20, col=grey(0.7), cex=1.5)

#######################################
## add horizontal lines to visually seperate each season
#######################################
segments(1663, 13, 1930, 13, lty=2, col="darkgrey", lwd=1)
segments(1663, 26, 1930, 26, lty=2, col="darkgrey", lwd=1)
segments(1663, 39, 1930, 39, lty=2, col="darkgrey", lwd=1)

#############################################
## 7) add moving median
#############################################

##p.median <- moving_median_in_weeks(p, 9)
p.median <- moving_median_in_weeks(p, 21)
##lines(peak.year+0.5, p.median, col=jet.colors(200)[200], cex=0.5)
special.year <- 1838
peak.year.early <- peak.year[peak.year < special.year]
p.median.early <- p.median[peak.year < special.year]
peak.year.late <- peak.year[peak.year > special.year]
p.median.late <- p.median[peak.year > special.year]
## lines(peak.year.early+0.5, p.median.early, col="black", cex=0.5)
lines(peak.year.early+0.5, p.median.early, col=jet.colors(200)[185], lwd=3)
lines(peak.year.late+0.5, p.median.late, col=jet.colors(200)[185], lwd=3)
## points(peak.year+0.5, p.median, pch=15, col=jet.colors(200)[200], cex=1)

### Testing Ben's idea of plotting annual moving average for peaks
### Decided to NOT go with this represntation
#p.annual <- seq(peak.year[1], peak.year[length(peak.year)])
#p.annual <- as.data.frame(p.annual)
#p.data <- as.data.frame(cbind(peak.year, p))
#p.every.year <- merge(p.data, p.annual, by.x="peak.year", by.y="p.annual", all=TRUE)
#p.every.year.median <- moving_median_in_weeks(p.every.year$p, 21)
#points(p.every.year$peak.year+0.5, p.every.year.median, type="p", pch=4, col="blue", cex=0.5)

## panel label
if (show.panel.labels) legend(position.panel.label, legend=" D", bty="n", cex=cex.panel.label)

#############################################
## 8) insert timeline of historical events
#############################################
par(mar=c(0,0,0,1));

## set up plot area
plot(numdate, smpx, type="n", 
     xlim=c(1648, numdate[length(numdate)]),
     ylim=c(-0.3, -0.06),
     xlab="", ylab="", main="",  
     axes=FALSE);

## insert timeline
setup_timeline(variable = smpx, cex.text=1.1)

if (!interactive())
  dev.off()

  
