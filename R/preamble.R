# Common packages, functions and themes for charts/tables

# Set Global Options
options(stringsAsFactors=FALSE, 
        digits=4,
        scipen=999)

# Packages -------------------------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(xtable))


# Functions ----------------------------------------------------------------

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
    )
}

na_zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

na_missing <- function (x) {
  x[is.na(x)] <- "Missing"
  return(x)
}

trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)

clean_text <- function (x){
  x <- gsub("[!@#$%^&()/*?<>.,\\\\]", "", x)
  return (x)
}

# ggplot theme and colours --------------------------------------------------------------
# For custom board colours

gedsbGreen <- "#59AD46"
gedsbBlue <- "#04559F"
gedsbGreen2 <- "#8CE079"
gedsbBlue2 <- "#51A2EC"


# Colour Palette - GEDSB Colours for charts -------------------------------------------------

palette3 <- c("#59AD46", "#173B32", "#04559F")
palette4 <- c("grey60","#59AD46","#04559F", "#8ecb80")
palette5 <- c("#04559F", "#1E6FB9", "#3788D2", "#51A2Ec", "#6ABBFF")
palette5 <- c("#6ABBFF", "#51A2Ec", "#3788D2",  "#1E6FB9","#04559F" )


# ggplot Theme ------------------------------------------------------------

theme_update(
  plot.margin= unit(c(0.25,0.25,0.25,0.25), "cm"),
  plot.title = element_text (colour="black", size=12,hjust = 0.5),
  plot.subtitle = element_text (colour="black", size=10,hjust = 0.5),
  
  panel.background = element_rect(fill="NA"),
  panel.border = element_blank(),
  panel.spacing = unit(1, "lines"),
  
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
  

