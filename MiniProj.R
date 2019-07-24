##SESYNC Summer Institite, Mini-Project##
##Authors: Erin Bunting and Liz Mack##

##Set working directory##
setwd("Z:/ResearchBunting/Conflict_Zones/Working/")

##Packages##
install.packages("shiny")
install.packages("rgdal")
install.packages("maptools")
install.packages("raster")
install.packages("ggplot2")
install.packages("spatstat")
install.packages("shapefiles")
install.packages("reshape2")
install.packages("sp")

library(shiny)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)
library(spatstat)
library(shapefiles)
library(reshape2)
library(sp)

##Read in the Conflict Data##
ACLED<-read.csv("ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv")

##Read in the shapefiles, like African country boundaries##
Africa<-readOGR(".", "Africa")

par(mar=c(1,1,1,1))
plot(Africa) 

Africa      

##Add column to ACLED called region and another called decade##
ACLED$Region<-""
  
ACLED$Decade<-""


##Display X,Y Data
projection_info<-crs(Africa)

ACLED_pt <- SpatialPointsDataFrame(ACLED[,1:2], #change to col numbers for XY in excel
                                            ACLED,    
                                            proj4string = projection_info)

plot(ACLED_pt, main=NULL, cols=rgb(0,0,0,.2), pch=20)
plot(Africa, add=TRUE)

##Point density per country

  
  