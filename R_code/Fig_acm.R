##
## revised by David, 30 June 2020

## Fig_acm.R:
## create Fig: 
## Weekly all-cause deaths and its trend; London, England, 1661-1930. 
## The trend was estimated by Empirical Mode Decomposition 
## applied separately to the periods 1661-1842 and 1842-1930, 
## which correspond to different data sources. 
## The largest peak of all-cause deaths occurred during the 
## Great Plague of London (1664-1665), 
## which at its peak killed over 8,000 people in one week.
  
## set working directoty to the source file location
if (interactive()) 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## clear all:
rm(list=ls())

## load required packages
library(plotrix)

## load functions: 
source("functions_for_plots.R")

## load the data file:
##data<-read.csv("../Data/master_file_no_mv.csv", header=TRUE, sep = ",", comment.char="#")
data<-read.csv("../Data/London_smallpox.csv", header=TRUE, sep = ",", comment.char="#")

## define data to work with:
## cleaned acm data (no missing values, no heap weeks)
acm<-data$acm.no.uhp
## numdate for x-axis
numdate <- data$numdate;

library(dplyr)
library(lubridate)
dd <- data.frame(numdate, acm)
## Great Plague of London:
gp <- dd %>% filter(numdate > 1664 & numdate < 1666)
(gp.max.date <- gp %>% filter(acm > 8000) %>% mutate(date=date_decimal(numdate)))
## 1918 flu:
flu <- dd %>% filter(numdate > 1917 & numdate < 1920)
(flu.max.dates <- flu %>% filter(acm > 4000) %>% mutate(date=date_decimal(numdate)))
## less than flu:
ltflu <- dd %>% filter(acm < 4000) %>% mutate(date=date_decimal(numdate))
ltflu.max <- ltflu %>% filter (acm==max(acm))

#install.packages("extrafont")
#library("extrafont")
#font_import()
#loadfonts()
#fonts()

##if (!interactive()) 
pdf("../Figures_and_Tables/Fig_acm.pdf", family = "ArialMT", width=11, height=7)
#quartz(width=11, height=7)

#######################################
## 1) set up plot area
##    with the gap for the plague deaths
#######################################
## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
par(mar = c(2.5,5.5,1,1))
plot(numdate, acm,
     las=1,
     cex.axis=1.25,
     cex.lab=1.25,
     lab=c(10,10,10),
     ##mgp = c(3, 1, 0), # default
     mgp = c(4, 1, 0),
     ##gap=c(4300,7500), gap.axis="y", breakcol="white", 
     type="l", main="", col="black", 
     ##brw=0.017,  
     ##xtics=seq(1660, 1930, by=20), xticlab=seq(1660, 1930, by=20),  
     ##ytics=c(seq(0, 4000, by=1000), 8000), yticlab=c(seq(0, 4000, by=1000), 8000),  
     xaxs="i", yaxs="i", 
     xlab="", ylab="Weekly all-cause deaths",
     ##xlim=c(1661, 1931),
     ylim=c(0,4000)
     )

## add axis break
##axis.break(2, 4300)

## add a line on the right hand side 
## so there is no break on the plot border
segments(1941.7,-200, 1941.7,5750, lty=1, lwd=2)

#######################################
## 2) add data sources
#######################################
## London Bills of Mortality: 1660-1842
data_source (numdate, 4000,
             left.arrow.start = 1660,
             left.arrow.end = 1725,
             right.arrow.start = 1782,
             right.arrow.end = 1842,
             #y.coordinate = 5150,
             ## this does not behave as expected because she multiplies by 1.284:
             y.coordinate = 3880/1.284, 
             text.here = "  London Bills of Mortality",
             cex.text = 1,
             background.colour = rgb(224/255,238/255,238/255,alpha=0.5))

## Registrar General's Weekly Returns
data_source (numdate, 4000,
             left.arrow.start = 1842,
             left.arrow.end = 1865,
             right.arrow.start = 1910,
             right.arrow.end = 1931,
             #y.coordinate = 5150,
             y.coordinate = 3880/1.284,
             text.here = "Registrar General's\nWeekly Returns",
             cex.text = 1,
             background.colour = rgb(224/255,238/255,224/255,alpha=0.5))

## add line to visually seperate data sources
segments(1842,-200, 1842,5300, lty=3, lwd=1, col="black")
## add line to x-axis so background colour doesn't cover the x-axis
segments(1650,0, 1941.7,0, lty=1, lwd=2)

#######################################
## 3) add acm trend data
#######################################
lines(numdate, data$acm.trend, col="black", lwd=2)

if (!interactive()) 
dev.off()

gp.max.date
flu.max.dates
ltflu.max
