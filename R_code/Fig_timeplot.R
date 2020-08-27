## Fig_timeplot.R:
## create Fig: 
## London's population and weekly smallpox deaths 1664-1930 
## against the timeline of historical events related to the 
## history of smallpox in England.

## set working directoty to the source file location
if (interactive()) 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## clear all:
rm(list=ls())

## load functions: 
source("functions_for_plots.R")

## load the data files:
## mortality data file
##data <- read.table("../Data/master_file_no_mv.csv", header=TRUE, sep = ",");
data <- read.table("../Data/London_smallpox.csv", header=TRUE, sep = ",");
## population data file
pop.data <- read.table("../Data/LondonPopulation1550_1931annual.csv", 
                       header=TRUE, sep = ",");


## define data to work with:
## raw smallpox data with missing values and UHP weeks, not cleaned
smpx <- data$smpx;
## numdate for x-axis
numdate <- data$numdate;

## for the population data define:
## year
year <- pop.data[,"Year"];
## total population of London
tot.lon <- pop.data[,"Total"];
## population of Inner Londond
inn.lon <- pop.data[,"InnerLondon"];

## smallpox was recorded under flox and smallpox column until 1701.12.09
## define index for flox.and.smallpox data
## this is used when showing data sources
end.fl.sm <- which(data$year.from==1701&data$month.from==12&data$day.from==9);

###################
## PLOT
###################
## new plotting window
#quartz(width=10, height=12)

## to save figure in pdf
pdf("../Figures_and_Tables/Fig_timeplot.pdf", family = "ArialMT", width=10, height=12)

layout(matrix(c(1,2),2,1), height=c(8,4))
par(mar=c(0,0,0,0.5)); # mar=c(bottom, left, top, right)

#######################################
## 1) set up plot area
#######################################
setup_plot(numdate, smpx)

######################################
## 2) insert background and text
##    to show different data sources
######################################
## insert text for data sources
text(1650, 370, "Data 
sources", cex=0.7, srt=90, font=2);

## data sources included:
## i) London Bills of Mortality
## - smallpox deaths were recordedfrom 1664 until 1701 under the name \flox and smallpox" 
## use light blue background colour
data_source(numdate, smpx,
                    left.arrow.start = 1664,
                    left.arrow.end = 1667,
                    right.arrow.start = numdate[end.fl.sm]-3,
                    right.arrow.end = numdate[end.fl.sm],
                    text.here = "London bills of mortality:
  flox and smallpox",
                    cex.text =0.7,
                    background.colour = colors()[15])

## i) London Bills of Mortality
## - under the name \smallpox" from 1701 until 1842
## use light yellow background colour
data_source(numdate, smpx,
                    left.arrow.start = numdate[end.fl.sm],
                    left.arrow.end = 1754.5,
                    right.arrow.start = 1788,
                    right.arrow.end = 1842,
                    text.here = "London bills of mortality:
  smallpox",
                    cex.text =0.7,
                    background.colour = "honeydew")

## ii) Registrar General's Weekly Returns used from 1842 until 1931
## use light grey background colour
data_source(numdate, smpx,
                    left.arrow.start = 1842,
                    left.arrow.end = 1875,
                    right.arrow.start = 1900,
                    right.arrow.end = 1931,
                    text.here = "Registrar General's
  Weekly Returns",
                    cex.text =0.7,
                    background.colour = "honeydew2")

######################################
## 3) plot raw smallpox time series
######################################
lines(numdate, smpx, lwd=0.55, lty=1, col="darkblue");

#############################################
## 4) insert timeline of historical events
#############################################
setup_timeline(variable = smpx, cex.text = 0.7)

#############################################
## 5) insert intervention uptake levels
##    as coloured bars: 
##    yellow green-dark olive for variolation 
##    with yellow green indicating the lowest level and dark olive the highest; 
##    and yellow-red for vaccination with light yellow indicating the lowest level 
##    and red the highest level.
#############################################
## m - reference y-coordinate
m <- max(smpx[!is.na(smpx)])

text(1650, 1.128*m, "Intervention
uptake 
level", cex=0.7,srt=0, col="black", font=2);

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
variolation_bar(smpx, col_bar=green_pallet(), magnification_par=1.28);

## VACCINATION
## insert vaccination bar
vaccination_bar(smpx, col_bar=heat.colors(40)[30:1], magnification_par=1.24);

#############################################
## 6) insert population plot
#############################################
London_pop(variable = smpx, year = year[115:382], 
                       tot.lon = tot.lon[115:382], inn.lon = inn.lon[115:382])

#############################################
## 7) add trend of weekly births
##    on log_10 scale
#############################################
birth_trend();

#############
#############
#############

par(mar=c(2,0,0,0.5)); # mar=c(bottom, left, top, right)

## (cleaned: no missing values, no entry types, no UHP)
smpx <- data$smpx.norm;
## numdate for x-axis
numdate <- data$numdate;

#######################################
## 1) set up plot area
#######################################
setup_plot(numdate, variable=smpx, 
           y.ticks=c(0, 0.1, 0.2, 0.3, 0.4),
           ylim.vector=c(0, 0.4),
           y.text = 0.23,
           text.here="Normalized smallpox deaths",
           axis.pos = c(-0.014,1663),
           cex.axis=0.7)

######################################
## 2) insert background and text
##    to show different data sources
######################################
## insert text for data sources
text(1650, 1.284*max(smpx[!is.na(smpx)]), "Data 
     sources", cex=0.7, srt=90, font=2);

## data sources included:
## i) London Bills of Mortality
## - smallpox deaths were recordedfrom 1664 until 1701 under the name \flox and smallpox" 
## use light blue background colour
data_source(numdate, smpx,
            left.arrow.start = 1664,
            left.arrow.end = 1667,
            right.arrow.start = numdate[end.fl.sm]-3,
            right.arrow.end = numdate[end.fl.sm],
            text.here = "London bills of mortality:
            flox and smallpox",
            cex.text =0.6,
            background.colour = colors()[15])

## i) London Bills of Mortality
## - under the name \smallpox" from 1701 until 1842
## use light yellow background colour
data_source(numdate, smpx,
            left.arrow.start = numdate[end.fl.sm],
            left.arrow.end = 1754.5,
            right.arrow.start = 1788,
            right.arrow.end = 1842,
            text.here = "London bills of mortality:
            smallpox",
            cex.text =0.6,
            background.colour = "honeydew")

## ii) Registrar General's Weekly Returns used from 1842 until 1931
## use light grey background colour
data_source(numdate, smpx,
            left.arrow.start = 1842,
            left.arrow.end = 1875,
            right.arrow.start = 1900,
            right.arrow.end = 1931,
            text.here = "Registrar General's:
            Weekly Returns",
            cex.text =0.6,
            background.colour = "honeydew2")

######################################
## 3) plot normlized smallpox time series
##    and its trend
######################################
## normalized smallpox
lines(numdate, smpx, lwd=0.55, lty=1, col="darkblue");
## trend of normalized smallpox
lines(numdate, data$smpx.norm.trend, type="l", 
      lwd=2, lab=c(25, 10, 7), tck=0.01, mgp=c(3, 1, 0))

####################################################
## 7) identify largets epidemics of the 19th century
###################################################
## EPIDEMIC of 1837
p1 <- which(floor(data$numdate)==1837)[1];
p2 <- which(floor(data$numdate)==1839)[1];
a <- max(data$smpx.norm[p1:p2]);
indx <- which(data$smpx.norm==a);
points(numdate[indx], smpx[indx], pch=20, col="red", cex=0.8);

## EPIDEMIC of 1871
p1 <- which(floor(data$numdate)==1871)[1];
p2 <- which(floor(data$numdate)==1873)[1];
a <- max(data$smpx.norm[p1:p2]);
indx <- which(data$smpx.norm==a);
points(numdate[indx], smpx[indx], pch=20, col="red", cex=0.8);

if (interactive())
dev.off()
