## load packages:
library(fields);

#####################
# setup_plot()
#####################
#' set up plot area and axis
#'
#' @param numdate x-axis time variable
#' @param variable y-axis variable 
#' @param y.ticks vector of y-ticks coordinates 
#' @param y.labels vector of y-labels 
#' @param ylim.vector -yaxis limits
#' @param y.text y-axis coordinate to position y-axis label/text
#' @param y.axis.pos x-axis position at which y axis to be drawn
#' @param text.here y-axis label
#' @param cex.axis magnification for axis annotation
#' @param y.colour colour of the y-axis and y-axis label
#' @param y.las determines the y axis ticks rotation, 2 - perpendicular to the axis
#'@param  h.adj position of values for y ticks
#' 
setup_plot <- function(numdate, variable = smpx, 
                       y.ticks=c(0, 50, 100, 150, 200, 250, 290),
                       y.labels = y.ticks,
                       ylim.vector=c(-201, 495),
                       y.text = 170,
                       axis.pos = c(-10,1663),
                       text.here="Weekly smallpox deaths",
                       cex.axis=0.7,
                       y.colour="darkblue",
                       y.las=2,
                       h.adj=1)
{
  ## m - reference y-coordinate
  m <- max(variable[!is.na(variable)])
  ## set up plot area
  plot(numdate, variable,    
     type="n",                                      # empty plot 
     xlim=c(1648, numdate[length(numdate)]),        # x-axis range
     ylim=ylim.vector,                              # y-axis range
     xlab="", ylab="", main="",                     # no labels
     axes=FALSE)                                    # no axis
  
  ## set up axis
  ## x-axis
  x.ticks <- c(1664, 1670, 1680, 1690, 1700, 1710, 
               1721, 1728, 1740, 1746, 1750, 1762, 1768, 1780, 1790, 1796, 1800, 
               1808, 1820, 1830, 1840, 1853, 1860, 1867, 1871, 1881, 1891,1898, 1910, 
               1924, 1931, 1914, 1918); # vector of significant historical years for timeline
  axis(1,                               # location: 1 = below the plot
       at=x.ticks,                      # points to draw tickmarks
       labels=floor(x.ticks),           # axis labels
       las=2,                           # style of axis labels. 2=always perpendicular to the axis
       cex.axis=cex.axis,               # magnification for axis annotation
       tck=-0.01,                       # length of tickmakrs
       mgp=c(3,0.5,0),                  # margin line for the axis title mgp = c(3,1,0) is default
       pos=axis.pos                     # position at which axis to be drawn
  );
  ## add extra ticks every 5 years - DOESN'T LOOK GOOD
   axis(1,                               # location: 1 = below the plot
        at=seq(1670,1930, by=10),
        # points to draw tickmarks
        tck=-0.01,                       # length of tickmakrs
        labels = FALSE,
        mgp=c(3,0.5,0),                  # margin line for the axis title mgp = c(3,1,0) is default
        pos=axis.pos                     # position at which axis to be drawn
   );
  ## y-axis
  axis(2,                              # location: 2 = left
       at=y.ticks,                     # points to draw tickmarks
       labels=y.labels,                # axis labels
       las=y.las,                          # style of axis labels. 2=always perpendicular to the axis
       cex.axis=cex.axis,              # magnification for axis annotation
       tck=-0.01,                      # length of tickmakrs
       mgp=c(3,0.7,0),                 # margin line for the axis title mgp = c(3,1,0) is default
       pos=axis.pos[2:1],              # position at which axis to be drawn
       col=y.colour,                   # colour for the axis line
       col.axis=y.colour,              # colour for the axis ticks
       lwd=1.5,                        # line width for the axis
       font=2,                         # font for text
       hadj=h.adj                      # position of values for y ticks
       ); 
  text(1650,  y.text, text.here, cex=cex.axis, srt=90, # srt = string rotation in degrees
       col=y.colour, font=2);
}


#############################
# data_source()
#############################
#' insert background, arrows and text 
#' to identify data sources
#'
#' @param numdate x-axis time variable
#' @param variable y-axis variable 
#' @param left.arrow.start  
#' @param left.arrow.end  
#' @param right.arrow.start 
#' @param right.arrow.end  
#' @param text.here text to identify data source 
#' @param cex.text magnification of text
#' @param y.coordinate height of annotation
#' @param background.colour

data_source <- function(numdate, variable = smpx, 
                       left.arrow.start = 1664,
                       left.arrow.end = 1667,
                       right.arrow.start = numdate[end.fl.sm]-3,
                       right.arrow.end = numdate[end.fl.sm],
                       text.here = "London bills of mortality:
flox and smallpox",
                       cex.text = 0.6,
                       y.coordinate = NULL,
                       background.colour = colors()[15])
{
  ## m - reference y-coordinate
  m <- if (is.null(y.coordinate)) max(variable[!is.na(variable)]) else y.coordinate
  ## insert background colour
  rect(left.arrow.start, -0.03*m, right.arrow.end, 1.326*m, 
       col=background.colour, border=NA);
  
  ## insert text and arrows
  arrows(left.arrow.start, 1.284*m, 
         left.arrow.end, 1.284*m, 
         length = 0.12, angle = 5, code = 1, 
         col = par("fg"), lty = par("lty"), lwd = 1);
  
  arrows(left.arrow.start, 1.284*m, 
         left.arrow.end, 1.284*m, 
         length = 0.05, angle = 90, code = 1, 
         col = par("fg"), lty = par("lty"), lwd = 1);
  
  text(left.arrow.start+(right.arrow.end-left.arrow.start)/2, 1.284*m, 
       text.here, cex=cex.text);
  
  arrows(right.arrow.start, 1.284*m, 
         right.arrow.end, 1.284*m, 
         length = 0.12, angle = 7, code = 2,
         col = par("fg"), lty = par("lty"), lwd = 1);
  
  arrows(right.arrow.start, 1.284*m, 
         right.arrow.end, 1.284*m, 
         length = 0.05, angle = 90, code = 2,
         col = par("fg"), lty = par("lty"), lwd = 1); 
}

#####################
# setup_timeline()
#####################
#' set up timeline 
#' 
#' The timeline of historical events
#' related to smallpox history in England:
#' black text indicates events that influenced
#' uptake of control measures; 
#' brown text shows events that influenced human behaviour;
#' dark green shows the period when data accuracy was reduced.
#' 
#' @param variable y-axis variable
#' @param cex.text magnification for text annotation  

setup_timeline <- function(variable = smpx, cex.text = 0.7)
{
  ## m - reference y-coordinate
  m <- max(variable[!is.na(variable)])
  
  text(1650,-0.312*m , "Timeline", srt=90, cex=cex.text, font=2);
  ## 1721: Introduction of variolation
  arrows(1721,-0.156*m, 1721, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1704, -0.295*m, "Introduction 
of variolation", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1746: London Smallpox and Inoculation Hospital founded
  arrows(1746,-0.156*m, 1746, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1727, -0.295*m, "London Smallpox 
and Inoculation 
Hospital founded", cex=cex.text, las=1,adj=c(0,1));
  

  ## 1760: Industrial revolution
  arrows(1760,-0.163*m, 1830, -0.163*m, 
         length = 0.05, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col="chocolate4");
  arrows(1780,-0.165*m, 1780, -0.538*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  text(1772, -0.556*m, "Industrial
revolution", cex=cex.text, las=1, adj=c(0,1),col="chocolate4");
  
  ## 1762: Suttonian innovation
  arrows(1762,-0.156*m, 1762, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1758, -0.295*m, "Suttonian
innovation", cex=cex.text, las=1, adj=c(0,1));
  
  
  ## 1790: Progressive collapse of the parish registration system
  ##arrows(1790,-0.18*m, 1841, -0.18*m, 
  arrows(1790,-0.187*m, 1841, -0.187*m, 
         length = 0.05, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col="darkgreen");
  arrows(1820,-0.187*m, 1820, -0.38*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="darkgreen");
  arrows(1820,-0.42*m, 1820, -0.538*m, 
         length = 0.15, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="darkgreen");
  text(1810, -0.556*m, "Progressive 
collapse 
of the parish
registration 
system", cex=cex.text, las=1, adj=c(0,1),col="darkgreen");
  
  
  ## 1796: Vaccine discovery
  arrows(1796,-0.156*m, 1796, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1785, -0.295*m, "Vaccine
discovery", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1803-1815: Napoleonic Wars
  ##hh <- -0.205*m  # height of arrow
  hh <- -0.212*m  # height of arrow
  arrows(1803, hh, 1816, hh, 
         length = 0.05, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col="chocolate4");
  arrows(1806, hh - 0.002*m, 1801, hh - 0.093*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  arrows(1801, hh - 0.093*m, 1801, hh - 0.326*m,
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  text(1791, hh - 0.344*m, "Napoleonic
wars", cex=cex.text, las=1, adj=c(0,1),col="chocolate4");
  
  ## 1808: National Vaccine Establishment founded
  arrows(1808,-0.156*m , 1808, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1803,  -0.295*m, "National 
Vaccine
Establishment 
founded", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1840: Variolation banned, vaccination free of charge
  arrows(1840,-0.156*m, 1836, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1828, -0.295*m, "Variolation 
banned,
vaccination 
free of 
charge", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1853: Compulsory infant vaccination
  arrows(1853,-0.156*m , 1853, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1849, -0.295*m, "Compulsory 
infant
vaccination", cex=cex.text, las=1, adj=c(0,1));
  
  ## Franco-Prussian War
  FPW <- which(data$year.from==1870 & data$month.from==7)[3];
  FPW2 <- which(data$year.from==1871 & data$month.from==5)[2];
  arrows(data$numdate[FPW],-0.163*m, data$numdate[FPW2], -0.163*m, 
         length = 0.03, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col="chocolate4");
  ##arrows(data$numdate[FPW2], -0.156*m, 1873, -0.278*m, 1,
  arrows(data$numdate[FPW2], -0.18*m, 1873, -0.278*m, 1,
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  arrows(1873, -0.278*m,1873,-0.417*m, 1,
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  arrows(1873, -0.510*m,1873,-0.538*m, 1,
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  text(1867, -0.556*m, "Franco-
Prussian 
war", cex=cex.text, las=1, adj=c(0,1), col="chocolate4");
  
  ## 1867: Strict enforcement of the vaccination laws
  arrows(1867,-0.156*m, 1869, -0.278*m,
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  ##arrows(1869,-0.278*m, data$numdate[FPW], -0.16*m, 1,
  arrows(1869,-0.278*m, data$numdate[FPW], -0.18*m, 1,
         length = 0.15, angle = 7, code = 2, 
         lty = par("lty"), lwd = 1, col="black");
  arrows(1869, -0.278*m,1869,-0.417*m, 1,
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1852, -0.434*m, "Strict enforcement
of vaccination laws", cex=cex.text, las=1, adj=c(0,1))
  
  ## 1881: New method of vaccine distribution
  arrows(1881,-0.156*m, 1881, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1874, -0.295*m, "New method 
of vaccine 
distribution", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1891: Adding glycerine to vaccine
  arrows(1891,-0.156*m, 1895, -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  arrows(1895,-0.278*m, 1895, -0.417*m, 
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="black");
  text(1891,  -0.434*m, "Adding glycerine 
to vaccine", cex=cex.text, las=1, adj=c(0,1));
  
  ## 1898: Arm-to-arm vaccination banned
  arrows(1898,-0.156*m, 1902, -0.278*m, 
         length = 0.15, angle = 7, code = 1,
         lty = par("lty"), lwd = 1, col="black");
  text(1898,  -0.295*m, "Arm-to-arm
vaccination 
banned", cex=cex.text, las=1, adj=c(0,1));
  
  ## World War I
  WW1 <- which(data$year.from==1914 & data$month.from==6)[4];
  WW12 <- which(data$year.from==1918 & data$month.from==11)[2];
  arrows(data$numdate[WW1],-0.163*m, data$numdate[WW12], -0.163*m, 
         length = 0.04, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col="chocolate4");
  arrows(1916.5,-0.163*m, data$numdate[WW12], -0.278*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  arrows(data$numdate[WW12],-0.278*m, data$numdate[WW12], -0.538*m, 
         length = 0, angle = 0, code = 1, 
         lty = par("lty"), lwd = 1, col="chocolate4");
  text(1914, -0.556*m, "World
War I ", cex=cex.text, las=1, adj=c(0,1), col="chocolate4");
  
  ## 1920: Variola minor
  variola.minor.col <- "darkgreen" # "chocolate4"
  arrows(1920,-0.163*m, 1931, -0.163*m, 
         length = 0.04, angle = 90, code = 3, 
         lty = par("lty"), lwd = 2, col=variola.minor.col);
  arrows(1928,-0.163*m, 1928, -0.538*m, 
         length = 0.15, angle = 7, code = 1, 
         lty = par("lty"), lwd = 1, col=variola.minor.col);
  text(1926, -0.556*m, "Variola
minor only", cex=cex.text, las=1, adj=c(0,1), col=variola.minor.col);
  #
}

#####################
# colour_bar()
#####################
#' create colour_bar for 
#' increasing variolation and vaccination levels
#'
#' @param start.year start x-axis point for the bar (left corner)
#' @param end.year end x-axis point for the bar (right corner) 
#' @param top_y_axis top corner coordinate
#' @param bottom_y_axis bottom corner coordinate 
#' @param plot_parameters use par("usr")=values for the corner corrdinates of the plot
#' @param col_bar pallet for the colour bar
#' @param magnification_par used to adjust bar length
#' 
## this function creates olieve pallet 
green_pallet <- function(){
  start <- as.vector(col2rgb(colors()[88]));
  end <- as.vector(col2rgb(colors()[89]));
  inter <- (start-end)/30;
  r <- seq(start[1], end[1], -inter[1])/255;
  g <- seq(start[2], end[2], -inter[2])/255;
  bl <- seq(start[3], end[3], -inter[3])/255;
  return(rgb(r,g,bl))
} 

colour_bar <- function(start.year, end.year, top_y_axis, 
                       bottom_y_axis, plot_parameters, 
                       col_bar=green_pallet(),
                       magnification_par=1.3)
{
  ## calculate bar length
  a <- (end.year-start.year)/(plot_parameters[2]-plot_parameters[1])*magnification_par;
  ## calculate bar width
  b <- (top_y_axis-bottom_y_axis)/(plot_parameters[4]-plot_parameters[3]);
  ## plot
  colorbar.plot((start.year+end.year)/2, (top_y_axis+bottom_y_axis)/2, 
                strip.length=a,strip.width=b,seq(1,300), col=col_bar);
}

#####################
# variolation_bar()
#####################
#' insert variolation uptake bar
#' @param variable y-axis variable
#' @param col_bar pallet for the colour bar
#' @param magnification_par used to adjust bar length 
#' @param cex.coeff used to magnify text annotation

variolation_bar <- function(variable = smpx, 
                            col_bar=green_pallet(), 
                            magnification_par=1.3,
                            cex.coeff=1
                            )
{
  ## m - reference y-coordinate
  m <- max(variable[!is.na(variable)])
  
  ## y-coordinates for the corner of the bar and text
  top_y_axis <- 1.213*m; 
  bottom_y_axis <- 1.128*m;
  center_y_axis <- 1.171*m;
  
  rect(1664, top_y_axis, 1721, bottom_y_axis, lty=1);
  text(1670, center_y_axis , adj=c(0,0.5),"Variolation",cex=0.8*cex.coeff, lwd=2, font=2);
  
  rect(1721, top_y_axis, 1728, bottom_y_axis, col=colors()[86]);
  text(1724.5, center_y_axis , "Very 
low", cex=0.5*cex.coeff, las=1);
  
  rect(1728, top_y_axis, 1740, bottom_y_axis, col=colors()[87]);
  text(1734, center_y_axis , "Low", cex=0.7*cex.coeff, las=1);
  
  rect(1740, top_y_axis, 1768, bottom_y_axis, col=colors()[88]);
  text(1754, center_y_axis , "Moderate", cex=0.7*cex.coeff, las=1);
  
  ## create colour_bar for 
  ## increasing variolation levels
  colour_bar(start.year=1768, 
             end.year=1790, 
             top_y_axis, 
             bottom_y_axis, 
             plot_parameters=par("usr"),
             col_bar,
             magnification_par)
  
  rect(1768, top_y_axis, 1790, bottom_y_axis);
  text(1779, center_y_axis, "Increasing", cex=0.7*cex.coeff, las=1)
  
  rect(1790, top_y_axis, 1808, bottom_y_axis, col=colors()[89]);
  text(1799, center_y_axis, "High", cex=0.7*cex.coeff, las=1)
  
  rect(1808, top_y_axis, 1840, bottom_y_axis, col=colors()[87]);
  text(1824, center_y_axis, "Low", cex=0.7*cex.coeff, las=1);
}

#####################
# vaccination_bar()
#####################
#' insert vaccination uptake bar
#' @param variable y-axis variable
#' @param col_bar pallet for the colour bar
#' @param magnification_par used to adjust bar length 
#' @param cex.coeff used to magnify text annotation

vaccination_bar <- function(variable = smpx, col_bar, 
                            magnification_par, cex.coeff = 1)
{
  ## m - reference y-coordinate
  m <- max(variable[!is.na(variable)])
  
  ## y-coordinates for the corner of the bar and text
  bottom_y_axis <- 1.042*m; 
  top_y_axis <- 1.128*m;
  center_y_axis <- 1.085*m;
  
  rect(1664, top_y_axis, 1808, bottom_y_axis, lty=1);
  text(1670, center_y_axis, "Vaccination",cex=0.8*cex.coeff, adj=c(0,0.5), lwd=2, font=2);
  
  rect(1796, top_y_axis, 1808, bottom_y_axis,  col=heat.colors(40)[38]);
  text(1802, center_y_axis, "Low", cex=0.7*cex.coeff, las=1)
  
  rect(1808, top_y_axis, 1840, bottom_y_axis, col=heat.colors(40)[34]);
  text(1824, center_y_axis, "Moderate", cex=0.7*cex.coeff, las=1);
  
  ## create colour_bar for 
  ## increasing vaccination levels
  colour_bar(start.year=1840, 
             end.year=1931, 
             top_y_axis, 
             bottom_y_axis, 
             par("usr"),
             col_bar,
             magnification_par);
  
  rect(1840, top_y_axis, 1931, bottom_y_axis);
  text(1885, center_y_axis, "Steadily increasing", cex=0.7*cex.coeff, las=1);
}

#####################
# London_pop()
#####################
#' insert London population graph
#' @param variable y-axis variable
#' @param year x-axis timeline for population plot
#' @param tot.lon population of London 
#' @param tot.lon population of Inner London
#' 
London_pop <- function(variable = smpx, year = year[115:382], 
                       tot.lon = tot.lon[115:382], inn.lon = inn.lon[115:382])
{
  ## m - reference y-coordinate
  m <- max(variable[!is.na(variable)])
  n <- 288
  
  ## set up y-axis
  y.ticks <- c(1.416*m, 1.694*m);
  y.labels <- c(0,"");
  axis(2, at=y.ticks, labels=y.labels, las=1, 
       cex.axis=0.6, tck=-0.01, mgp=c(0,0.5,0), 
       pos=c(1663));
  text(1650, 1.58*m, "London population 
(estimates)", srt=90, cex=0.7, font=2);
  
  ## set up x-axis
  pop.year<-c(1664, 1700, 1750, 1800, 1850, 1900, 1931);
  pop.year.inn <- c(1900, 1931)
  axis(3, at=pop.year, labels=pop.year, las=1, 
       cex.axis=0.6, tck=-0.007, mgp=c(0,-0.8,0), pos=c(1.416*m, 1663));
  
  ## add total London population line
  lines(year, (tot.lon/100000)/n*m+1.416*m, 
        type="l", lwd=2, lab=c(25, 10, 7), tck=0.01, mgp=c(3, 1, 0));
  
  ## add inner Londond population line
  lines(year, (inn.lon/100000)/n*m+1.416*m, lty=2);
  
  ## add points to show population each 50 years
  pop1 <- sapply(1:length(pop.year), 
                 function(i){
                   tot.lon[which(year==pop.year[i])]
                 });
  pop2 <- sapply(1:length(pop.year.inn), 
                 function(i){
                   inn.lon[which(year==pop.year.inn[i])]
                 });
  pop <- round(c(pop1, pop2),0);
  points(c(pop.year, pop.year.inn), (pop/100000)/n*m+1.416*m, 
         pch=20, col="red", cex=0.8);
  
  
  pop.year.new <- c(pop.year, pop.year.inn);
  pop.year.new[1] <- 1669;
  text(pop.year.new, (pop/100000)/n*m+1.442*m,
       labels=format(round(pop,-3), decimal.mark=",", big.mark=","), 
       cex=0.6, las=1);
  
  legend(1664,1.69*m, 
         c("Total London", "Inner London"), lty = c(1,2),cex=0.6);
}

#####################
# birth_trend()
#####################
#' add trend of weekly births on log_10 scale
#' 
#' We needed to plot births on the same graph as smallpox
#' but using different axis and using log10 scale (right side)
#' Idea: [min:max] is transformed from [0:2600] to [0:288]
#'       transformation parameter: k=2600/288, hence y=x/k
#'       in log10 scale it will be k=log10(2600)/288
#'       but in our case the y-axis starts from 10, so
#'       k=(log10(2600)-1)/288
#' 

birth_trend <- function()
{
  ## identify index for the year 1842
  ## this the year when data source switch from London bills to 
  ## Registrars Generals' retulrns 
  br <- which(floor(numdate)==1842)[1]+1;
  
  ## calculate parameter k for data transformation
  k <- (log10(2600)-1)/288;
  
  ## set up axis
  text(1941,150 , "Trend of weekly births", srt=-90, cex=0.7, col="darkred", font=2);
  y.ticks3 <- c((1-1)/k, (2-1)/k, (3-1)/k, 290);
  y.labels <- c(expression(10^1), expression(10^2),expression(10^3), "");
  axis(4, at=y.ticks3, 
       labels=y.labels, las=1, 
       cex.axis=0.6, tck=-0.007, mgp=c(0,0.5,0), 
       pos=c(1932), col="darkred", col.axis="darkred", lwd=1.5, font=2);
  
  ## plot birth trend from the beginning to 1842
  lines(numdate[193:br], (log10(data$birth.trend[193:br])-1)/k, col="darkred", lwd=1.5);
  
  ## plot birth trend from the 1842 to the end
  br2 <- br+1;
  lines(numdate[br2:length(numdate)], 
        (log10(data$birth.trend[br2:length(numdate)])-1)/k, col="darkred", lwd=1.5)
  
  ## THIS IS OMMITED as data sources have different coverage are
  ## so we can't simply fit the line
  
  ## plot fitted birth for the period 1798-1842 
  ## to connect two data sources
  ##fit.birth <- data$birth.trend.fitted;
  ##p1 <- which(floor(data$numdate)==1798)[1];
  ##p2 <- which(floor(data$numdate)==1842)[1];
  ##lines(data$numdate[p1:p2], (log10(fit.birth[p1:p2])-1)/k, col="darkred", lwd=1, lty=2);
}

#####################
# Moving_median_weeks()
#####################
#' calculates moving median for the variable "V" (always presented in weeks)
#' for a specified number of points "N"
#' @param V weeks for median calculations
#' @param N interval length for median claculation, always odd number
#' 
moving_median_in_weeks <- function(V = p, N = 9)
{
  ##% testing how circular() function works
  ##% need to mulitply by (360/52) to convert 1..52 scale to 1..360
  ##% necessary as circular function works either in degrees or radians
  ##% TEST
  ##% calculate median for a vector 
  ##% [1] 49 50 51 52  1
  ##% mod 52
  ##% if calculated correctly it should be 51, not 50
  
  ## x <- circular(360/52*c(49,50,51,52,1), type = "angles", 
  ##              units="degrees", template = "none", zero=0)
  ## plot(x) ## visual
  ##% calculate median
  ## z <- attributes(median(x))$median*52/360
  ## > z
  ## [1] 51
  if((N %% 2) == 0) {
    print(paste(N,"is Even, Enter Odd Number"))
    return()
  } else {
  
  ## Apply above formula to the peaks data
  v.circular <- circular(360/52*V, type = "angles", 
                         units="degrees", template = "none", zero=0)
  
  v.median <- array(NA, dim = length(V))
  for (i in (((N+1)/2):(length(V)-(N-1)/2))){
    v.median[i]<-attributes(median(v.circular[(i-(N-1)/2):(i+(N-1)/2)], na.rm = TRUE))$median*52/360
  }
  return(v.median)
  }
}

#' Transparent colour
#' @description
#' Given a colour, change it to a transparent version with a given \code{alpha} level
#' @inheritParams col2rgb
#' @param col as in \code{\link{col2rgb}}: vector of any of the three kinds of R color specifications, i.e., either a colour name (as listed by \code{colors()}), a hexadecimal string of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see \code{\link{rgb}}), or a positive integer \code{i} meaning \code{\link{palette}}\code{()[i]}.
#' @param alpha opacity level on scale from \code{0} to \code{1} (i.e., \code{1} means completely opaque)
#' @seealso \code{\link{col2rgb}}, \code{\link{rgb}}
#' @export
transparent_colour <- function(col,alpha=150/255) {
    alpha <- round(alpha * 255)
    v <- col2rgb(col)[,1] # color as rgb vector
    tcol <- rgb(v["red"],v["green"],v["blue"],alpha=alpha,maxColorValue = 255)
    return(tcol)
}

