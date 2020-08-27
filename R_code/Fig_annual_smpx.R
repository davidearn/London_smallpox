# Fig_annual_smpx.R 
#
# This file creates a time plot of annual smallpox and 
# smpx data from two different sources: Creighton and IIDDA library
# It has to be done for comparison
###################################
 if (interactive()) 
   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
data.Olga<-read.csv("../Data/annual_data.csv", header=TRUE, sep = ",", comment.char="#", colClasses=c("integer", "numeric",  "numeric", "numeric"))
data.Creighton<-read.table("../Data/Creighton_annual_smallpox.csv", header=TRUE, sep = ",", comment.char="#")
##data.weekly<-read.table("../Data/master_file_no_mv.csv", header=TRUE, sep = ",")
data.weekly <- read.csv("../Data/London_smallpox.csv", header=TRUE, sep = ",", comment.char="#")

# Identify the years with missing values:
missing.data<-data.weekly$numdate[which(is.na(data.weekly$smpx))]
missing.data<-floor(missing.data)
missing.data<-unique(missing.data)
index<-array(NA)
for (j in 1:length(missing.data))
	{
	for (i in 1:length(data.Olga$year))
	{
	if (data.Olga$year[i]==missing.data[j])
		{
			index[j]=i}
		}
	}

# Another attempt using barplot

if (!interactive())
  pdf("../Figures_and_Tables/Fig_annual_smpx.pdf", width=8, height=8, family = "ArialMT", paper="special")
#quartz(width=11, height=6)
par(mfrow=c(2,1)) # since we have a lot of data we split it into two images 150 years in each
# mar=c("bottom", "left", "top", "right") - defines margines in lines
## margins:  [default is c(b,l,t,r) = c(5, 4, 4, 2) + 0.1]
# mfg - defines number of the current figure on the plot
# lend=1 - makes lty="h" possible
# cex.axis - defines axis labels to be small
# tcl - length of the tick marks
# mgp=c("axis title", "axis labels", "axis line") - space between axis title,labels, and axis line
##par(mar=c(2,3,2,1),mfg=c(1,1),lend=1, cex.axis=0.7, tcl=-0.25, mgp=c(1,0.3,0))
par(mar=c(3,3,2,1),mfg=c(1,1),lend=1, cex.axis=1, tcl=-0.25, mgp=c(1,0.3,0))
m=floor((1930-1629)/2)
k=1629+m
c<-which(data.Creighton$year==k)
o<-which(data.Olga$year==k)
#
year.start<-data.Creighton$year[1]
data.Olga$year[1] # 1661
extra.years.Olga<-array(NA, (data.Olga$year[1]-data.Creighton$year[1]))
smpx.Olga<-data.Olga$smpx
smpx.Olga<-c(extra.years.Olga, smpx.Olga)
smpx.Olga[which(is.na(smpx.Olga))]<-0
year.end<-data.Olga$year[length(data.Olga$year)]
data.Creighton$year[length(data.Creighton$year)] # 1893
extra.years.Creighton<-array(NA, (data.Olga$year[length(data.Olga$year)]-data.Creighton$year[length(data.Creighton$year)]))
smpx.Creighton<-data.Creighton$smallpox.deaths
smpx.Creighton<-c(smpx.Creighton, extra.years.Creighton)
smpx.Creighton[which(is.na(smpx.Creighton))]<-0
diff<-array(NA)
diff<-(smpx.Creighton-smpx.Olga)
pos.d<-array(NA)
neg.d<-array(NA)
for (i in 1:length(diff))
{
	if (diff[i]>=0){pos.d[i]<-diff[i]
					neg.d[i]<-0}
	else {pos.d[i]<-0
			neg.d[i]<-diff[i]}
	}
neg.d[(c+(1894-1780)):length(neg.d)]<-0
smpx<-cbind(smpx.Creighton, smpx.Olga, pos.d, neg.d)
names <- c(1629, rep("", times=4), 1634, rep("", times=4), 1639, rep("", times=4), 1644, rep("", times=4), 1649, rep("", times=4), 1654, rep("", times=4), 1659, rep("", times=4), 1664, rep("", times=4), 1669, rep("", times=4), 1674, rep("", times=4), 1679, rep("", times=4), 1684, rep("", times=4), 1689, rep("", times=4),1694, rep("", times=4), 1699, rep("", times=4), 1704, rep("", times=4), 1709, rep("", times=4), 1714, rep("", times=4), 1719, rep("", times=4), 1724, rep("", times=4), 1729, rep("", times=4), 1734, rep("", times=4), 1739, rep("", times=4), 1744, rep("", times=4), 1749, rep("", times=4), 1754, rep("", times=4), 1759, rep("", times=4), 1764, rep("", times=4), 1769, rep("", times=4), 1774, rep("", times=4), 1779)

barplot(t(smpx[1:c,2:4]), beside=FALSE, space=0.5, col=c("red","white","black"), names.arg=names, 
        las=2, cex.names=c(1), cex.axis=c(1), axes=FALSE, tcl=0.25, 
        ylab="Smallpox deaths", cex.lab=1, ylim=c(0,4000), font=1)

axis(1, at=seq(1, 227.7, by=7.5), labels=rep("", times=length(seq(1, 227.7, by=7.5))), pos=c(-10), las=2)
axis(2, at=seq(0, 4000, by=1000), labels=seq(0, 4000, by=1000), pos=-1, las=2)

legend(1,4000, legend=c("Annual counts from Creighton [35]", "Annual sums of weekly data from this paper"), cex=1, fill=c("white", "red"), border=c("black", "black"), bty="n")

#
m=floor((1930-1629)/2)+1
k=1629+m
c<-which(data.Creighton$year==k)
o<-which(data.Olga$year==k)
##par(mar=c(2,3,2,1),mfg=c(2,1),lend=1, cex.axis=1, tcl=-0.25, mgp=c(1,0.3,0))
names <- c(1780, rep("", times=4), 1785, rep("", times=4), 1790, rep("", times=4), 1795, rep("", times=4), 1800, rep("", times=4), 1805, rep("", times=4), 1810, rep("", times=4), 1815, rep("", times=4), 1820, rep("", times=4), 1820, rep("", times=4), 1830, rep("", times=4), 1835, rep("", times=4), 1840, rep("", times=4),
1845, rep("", times=4), 1850, rep("", times=4), 1855, rep("", times=4), 1860, rep("", times=4), 1865, rep("", times=4), 1870, rep("", times=4), 1875, rep("", times=4), 1880, rep("", times=4), 1885, rep("", times=4), 1890, rep("", times=4), 1895, rep("", times=4), 1900, rep("", times=4), 1905, rep("", times=4), 1910, rep("", times=4), 1915, rep("", times=4), 1920, rep("", times=4), 1925, rep("", times=4), 1930)

barplot(t(smpx[(c):length(smpx[,1]),2:4]), beside=FALSE, space=0.5, 
        names.arg=names, las=2, cex.names=c(1), col=c("red","white","black"), 
        cex.axis=c(1), axes=FALSE, tcl=0.25, ylab="Smallpox deaths", cex.lab=1, ylim=c(0,4000))
axis(1, at=seq(1, 227.7, by=7.5), labels=rep("", times=length(seq(1, 227.7, by=7.5))), pos=c(-10), las=2)
axis(2, at=seq(0, 4000, by=1000), labels=seq(0, 4000, by=1000), pos=-1, las=2)

if (!interactive())
  dev.off()
