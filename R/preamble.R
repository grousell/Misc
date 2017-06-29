# Common packages, functions and themes for charts/tables

# Set Global Options
options(stringsAsFactors=FALSE, 
        digits=4,
        scipen=999)

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(knitr)
library(stringr)
library(foreign)
library(xtable)
library(lubridate)

# Functions

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
    )
}

na.Zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)

#------------------------------ ggplot theme and colours
# For custom board colours

gedsbGreen <- "#59AD46"
gedsbBlue <- "#04559F"
gedsbGreen2 <- "#8CE079"
gedsbBlue2 <- "#51A2EC"


# Colour Palette - GEDSB Colours for charts

palette3 <- c("#59AD46", "#173B32", "#04559F")
palette4 <- c("grey60","#59AD46","#04559F", "#8ecb80")
palette5 <- c("#04559F", "#1E6FB9", "#3788D2", "#51A2Ec", "#6ABBFF")
palette5 <- c("#6ABBFF", "#51A2Ec", "#3788D2",  "#1E6FB9","#04559F" )

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),
  title = element_text (colour="black", size=12),
  
  panel.background = element_rect(fill="NA"),
  panel.border = element_blank(),
  panel.margin = unit(1, "lines"),
  
  panel.grid.major.y = element_line(colour="grey90"),
  panel.grid.minor.y = element_line(colour="NA"),
  panel.grid.major.x = element_line(colour="NA"),
  panel.grid.minor.x = element_line(colour="NA"),
  
  axis.text.y = element_text (colour="black", size=10, hjust=1),
  axis.title.y = element_text (colour="black", size=12, angle=90),
  
  axis.text.x = element_text (colour="black", size=10,angle=0),
  axis.title.x = element_text (colour="black", size=12),
  
  axis.ticks = element_blank(),
  
  legend.text = element_text (colour="black", size = 12),
  legend.position = ("right"),
  legend.title = element_blank(),
  legend.key = element_blank()
  )
  

# Theme for tables 
tableTheme <- ttheme_default(
  # Use hjust and x to left justify the text
  # Alternate the row fill colours
  core = list(bg_params=list(fill=c("grey90", "#B6CBE9")),
              fg_params=list(hjust=0.5, x=0.5, fontface=1, cex=0.9)),
  rowhead=list(bg_params=list(fill=c("white", "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9",
                                     "grey90", "#B6CBE9", "grey90", "#B6CBE9","grey90", "#B6CBE9"), 
                              col = "white"),
               fg_params=list(hjust=0, x=0, fontface=1, cex=0.9)),
  # Change column header to white text and blue background
  colhead = list(fg_params=list(hjust=0.5, x=0.5,  col="white", fontface=2, cex=0.9),
                 bg_params=list(fill="#007CC2")))

