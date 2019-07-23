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

library(shiny)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)
library(spatstat)
library(shapefiles)
library(reshape2)

##Read in the Conflict Data##
ACLED<-read.csv("ACLED-Version-7-All-Africa-1997-2016_csv_dyadic-file.csv")

##Read in the shapefiles, like African country boundaries##
ACLED_pts <-readOGR(".", "ACLED_All_Africa19972018") #national_parks_boundaries_BTS_2006 to the name of your shapefile#

par(mar=c(1,1,1,1))
plot(ACLED_pts) #You should see your shapefile outline

ACLED_pts       #Shows the projection of your shapefile, make sure it is the same as the NDVI accum_files#