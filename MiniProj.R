##SESYNC Summer Institite, Mini-Project##
##Authors: Erin Bunting and Liz Mack##

##Set working directory##
setwd("/nfs/waterconflictafrica-data/Institute2019/Data/")
##setwd("~/Desktop/Michigan State University/South Africa H20 - General/Data/data_shortcut")
getwd()

##Packages##
##install.packages("shiny")
##install.packages("rgdal")
##install.packages("maptools")
##install.packages("raster")
##install.packages("ggplot2")
##install.packages("spatstat")
##install.packages("shapefiles")
##install.packages("reshape2")
##install.packages("sp")

library(shiny)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)
library(spatstat)
library(shapefiles)
library(reshape2)
library(sp)
library(readr)
##library(plyr)
library(dplyr)
library(sf)
library(rworldmap)
library(RColorBrewer)
library(rgeos)

##Read in the Conflict Data##
ACLED_Northern<-read.csv("ACLED9719_Northern_Africa.csv")
ACLED_Eastern<-read.csv("ACLED9719_Eastern_Africa.csv")
ACLED_Central<-read.csv("ACLED9719_Middle_Africa.csv")
ACLED_Western<-read.csv("ACLED9719_Western_Africa.csv")
ACLED_Southern<-read.csv("ACLED9719_Southern_Africa.csv")

ACLED_merge<-rbind(ACLED_Northern, ACLED_Eastern, ACLED_Central, ACLED_Western, ACLED_Southern)

##Read in the shapefiles, like African country boundaries##
Africa<-readOGR(".", "Africa")
plot(Africa) 
Africa 

#Fixing Names in the Africa attribute table#
##Africa@data[Africa@data$COUNTRY == "Cote d`Ivoire", "COUNTRY"] <- "Ivory Coast"

##Mapping points##
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-40, 60), ylim = c(0, 10), asp = 1)
points(ACLED_merge$longitude, ACLED_merge$latitude, col = "red", cex = .4, pch=19)

##Display X,Y Data
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

coords <- ACLED_merge[c("longitude","latitude")] 
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED_merge, proj4string = projection_info) 

plot(ACLED_pt, col = "red", cex = .4, pch=19)
plot(Africa, add=TRUE)

##Summary Statistics and mapping it back to the Africa shapefile##
CountALLConflictCountry<-count(ACLED_merge, c("country"))

CountConflictCountry<-count(ACLED_merge, c("event_type", "country"))

CountConflictRegion<-count(ACLED_merge, c("event_type", "Region"))

CountALLConflictCountry_joined <- merge(Africa, CountALLConflictCountry, by.x="COUNTRY", by.y="country")

Battles <- CountConflictCountry[ which(CountConflictCountry$event_type=='Battles'),]
RemoteViolence <- CountConflictCountry[ which(CountConflictCountry$event_type=='Explosions/Remote violence'),]
Protests <- CountConflictCountry[ which(CountConflictCountry$event_type=='Protests'),]
Riots <- CountConflictCountry[ which(CountConflictCountry$event_type=='Riots'),]
StrategicDev <- CountConflictCountry[ which(CountConflictCountry$event_type=='Strategic developments'),]
ViolenceCitizens <- CountConflictCountry[ which(CountConflictCountry$event_type=='Violence against civilians'),]

CountConflictCountry_joinedBattles <- merge(Africa, Battles, by.x="COUNTRY", by.y="country")
CountConflictCountry_joinedRemoteViolence <- merge(Africa, RemoteViolence, by.x="COUNTRY", by.y="country")
CountConflictCountry_joinedProtests <- merge(Africa, Protests, by.x="COUNTRY", by.y="country")
CountConflictCountry_joinedRiots <- merge(Africa, Riots, by.x="COUNTRY", by.y="country")
CountConflictCountry_joinedStrategicDev <- merge(Africa, StrategicDev, by.x="COUNTRY", by.y="country")
CountConflictCountry_joinedViolenceCitizens <- merge(Africa, ViolenceCitizens, by.x="COUNTRY", by.y="country")

spplot(CountALLConflictCountry_joined, "freq", main = "Country-Scale Event Country",  col = "transparent")

spplot(CountConflictCountry_joinedBattles, "freq", main = "Country-Scale Event Country--Battles",  col = "transparent")
spplot(CountConflictCountry_joinedRemoteViolence, "freq", main = "Country-Scale Event Country--Remote Violence",  col = "transparent")
spplot(CountConflictCountry_joinedProtests, "freq", main = "Country-Scale Event Country--Protests",  col = "transparent")
spplot(CountConflictCountry_joinedRiots, "freq", main = "Country-Scale Event Country--Riots",  col = "transparent")
spplot(CountConflictCountry_joinedStrategicDev, "freq", main = "Country-Scale Event Country--Strategic Development",  col = "transparent")
spplot(CountConflictCountry_joinedViolenceCitizens, "freq", main = "Country-Scale Event Country--Civilian Vio.",  col = "transparent")

######Point density per country 
#ACLED_pt, ACLED_merge
rasAf<-raster(Africa, res=.1)
extent(rasAf)<-extent(Africa)
rasAf<-rasterize(Africa, rasAf, 'COUNTRY')
plot(rasAf)

#Colors and Breaks for maps
my.palette <- brewer.pal(n = 10, name = "Spectral")
#
AllEvent_10kmDensity <- rasterize(coordinates(ACLED_pt), rasAf, fun='count', background=NA)

spplot(AllEvent_10kmDensity, 
       main = "All Conflict Count per 10km",
       col.regions = my.palette,
       cuts=10,
       style="quantile",
       col = "transparent")

##Density / Hotspot Analysis: All ACLED##
ACLED_ptBACKUP<-ACLED_pt

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_pt$PRESENT <- 1

#raster of All ACLED points
rasterACLED <- rasterize(ACLED_pt, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2<-log(rasterACLED)

#focal width / developing the kernel
kernel = focalWeight(rasterACLED2, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLED = focal(rasterACLED2, kernel, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygons = rasterToPolygons(x =heatALLACLED, n=8, dissolve=TRUE)

Allevent_polygons2<-Allevent_polygons

#bin values of the polygons
Allevent_polygons_class2 <- Allevent_polygons[Allevent_polygons$layer > 0 & Allevent_polygons$layer < 2,]
Allevent_polygons_class4 <- Allevent_polygons[Allevent_polygons$layer >= 2 & Allevent_polygons$layer < 4,]
Allevent_polygons_class6 <- Allevent_polygons[Allevent_polygons$layer >= 4 & Allevent_polygons$layer < 6,]
Allevent_polygons_class8 <- Allevent_polygons[Allevent_polygons$layer >= 6 & Allevent_polygons$layer < 8,]
Allevent_polygons_classgrt8 <- Allevent_polygons[Allevent_polygons$layer >= 8,]

Allevent_polygons2@data$layer <-ifelse (Allevent_polygons@data$layer >= 0 & Allevent_polygons@data$layer < 2, "2",
                                   ifelse (Allevent_polygons@data$layer >= 2 & Allevent_polygons@data$layer < 4, "4",
                                           ifelse(Allevent_polygons@data$layer >= 4 & Allevent_polygons@data$layer < 6, "6", 
                                                  ifelse (Allevent_polygons@data$layer >= 6 & Allevent_polygons@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLED)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2, add=T)


##Density / Hotspot Analysis: Battles##
##Density / Hotspot Analysis
ACLED_ptBattle = ACLED_pt[(ACLED_pt$event_type == 'Battles'),]

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_ptBattle$PRESENT <- 1

#raster of All ACLED points
rasterACLEDBattle <- rasterize(ACLED_ptBattle, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2Battle<-log(rasterACLEDBattle)

#focal width / developing the kernel
kernelBattle = focalWeight(rasterACLED2Battle, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLEDBattle = focal(rasterACLED2Battle, kernelBattle, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygonsBattle = rasterToPolygons(x =heatALLACLEDBattle, n=8, dissolve=TRUE)

Allevent_polygons2Battle<-Allevent_polygonsBattle

#bin values of the polygons
Allevent_polygons_class2Battle <- Allevent_polygonsBattle[Allevent_polygonsBattle$layer > 0 & Allevent_polygonsBattle$layer < 2,]
Allevent_polygons_class4Battle <- Allevent_polygonsBattle[Allevent_polygonsBattle$layer >= 2 & Allevent_polygonsBattle$layer < 4,]
Allevent_polygons_class6Battle <- Allevent_polygonsBattle[Allevent_polygonsBattle$layer >= 4 & Allevent_polygonsBattle$layer < 6,]
Allevent_polygons_class8Battle <- Allevent_polygonsBattle[Allevent_polygonsBattle$layer >= 6 & Allevent_polygonsBattle$layer < 8,]
Allevent_polygons_classgrt8Battle <- Allevent_polygonsBattle[Allevent_polygonsBattle$layer >= 8,]

Allevent_polygons2Battle@data$layer <-ifelse (Allevent_polygonsBattle@data$layer >= 0 & Allevent_polygonsBattle@data$layer < 2, "2",
                                              ifelse (Allevent_polygonsBattle@data$layer >= 2 & Allevent_polygonsBattle@data$layer < 4, "4",
                                                      ifelse(Allevent_polygonsBattle@data$layer >= 4 & Allevent_polygonsBattle@data$layer < 6, "6", 
                                                             ifelse (Allevent_polygonsBattle@data$layer >= 6 & Allevent_polygonsBattle@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLEDBattle)
plot(heatALLACLED, add=TRUE)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2Battle, add=T)

##Density/ Hotspot Analysis: Protests##
##Density / Hotspot Analysis
ACLED_ptProtests = ACLED_pt[(ACLED_pt$event_type == 'Protests'),]

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_ptProtests$PRESENT <- 1

#raster of All ACLED points
rasterACLEDProtests <- rasterize(ACLED_ptProtests, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2Protests<-log(rasterACLEDProtests)

#focal width / developing the kernel
kernelProtests = focalWeight(rasterACLED2Protests, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLEDProtests = focal(rasterACLED2Protests, kernelProtests, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygonsProtests = rasterToPolygons(x =heatALLACLEDProtests, n=8, dissolve=TRUE)

Allevent_polygons2Protests<-Allevent_polygonsProtests

#bin values of the polygons
Allevent_polygons_class2Protests <- Allevent_polygonsProtests[Allevent_polygonsProtests$layer > 0 & Allevent_polygonsProtests$layer < 2,]
Allevent_polygons_class4Protests <- Allevent_polygonsProtests[Allevent_polygonsProtests$layer >= 2 & Allevent_polygonsProtests$layer < 4,]
Allevent_polygons_class6Protests <- Allevent_polygonsProtests[Allevent_polygonsProtests$layer >= 4 & Allevent_polygonsProtests$layer < 6,]
Allevent_polygons_class8Protests <- Allevent_polygonsProtests[Allevent_polygonsProtests$layer >= 6 & Allevent_polygonsProtests$layer < 8,]
Allevent_polygons_classgrt8Protests <- Allevent_polygonsProtests[Allevent_polygonsProtests$layer >= 8,]

Allevent_polygons2Protests@data$layer <-ifelse (Allevent_polygonsProtests@data$layer >= 0 & Allevent_polygonsProtests@data$layer < 2, "2",
                                                ifelse (Allevent_polygonsProtests@data$layer >= 2 & Allevent_polygonsProtests@data$layer < 4, "4",
                                                        ifelse(Allevent_polygonsProtests@data$layer >= 4 & Allevent_polygonsProtests@data$layer < 6, "6", 
                                                               ifelse (Allevent_polygonsProtests@data$layer >= 6 & Allevent_polygonsProtests@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLEDProtests)
plot(heatALLACLED, add=TRUE)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2Protests, add=T)

#Density /Hotspot Analysis: Explosions / Remote Violence
##Density / Hotspot Analysis
ACLED_ptExplosionsRV = ACLED_pt[(ACLED_pt$event_type == 'Explosions/Remote violence'),]

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_ptExplosionsRV$PRESENT <- 1

#raster of All ACLED points
rasterACLEDExplosionsRV <- rasterize(ACLED_ptExplosionsRV, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2ExplosionsRV<-log(rasterACLEDExplosionsRV)

#focal width / developing the kernel
kernelExplosionsRV = focalWeight(rasterACLED2ExplosionsRV, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLEDExplosionsRV = focal(rasterACLED2ExplosionsRV, kernelExplosionsRV, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygonsExplosionsRV = rasterToPolygons(x =heatALLACLEDExplosionsRV, n=8, dissolve=TRUE)

Allevent_polygons2ExplosionsRV<-Allevent_polygonsExplosionsRV

#bin values of the polygons
Allevent_polygons_class2ExplosionsRV <- Allevent_polygonsExplosionsRV[Allevent_polygonsExplosionsRV$layer > 0 & Allevent_polygonsExplosionsRV$layer < 2,]
Allevent_polygons_class4ExplosionsRV <- Allevent_polygonsExplosionsRV[Allevent_polygonsExplosionsRV$layer >= 2 & Allevent_polygonsExplosionsRV$layer < 4,]
Allevent_polygons_class6ExplosionsRV <- Allevent_polygonsExplosionsRV[Allevent_polygonsExplosionsRV$layer >= 4 & Allevent_polygonsExplosionsRV$layer < 6,]
Allevent_polygons_class8ExplosionsRV <- Allevent_polygonsExplosionsRV[Allevent_polygonsExplosionsRV$layer >= 6 & Allevent_polygonsExplosionsRV$layer < 8,]
Allevent_polygons_classgrt8ExplosionsRV <- Allevent_polygonsExplosionsRV[Allevent_polygonsExplosionsRV$layer >= 8,]

Allevent_polygons2ExplosionsRV@data$layer <-ifelse (Allevent_polygonsExplosionsRV@data$layer >= 0 & Allevent_polygonsExplosionsRV@data$layer < 2, "2",
                                                    ifelse (Allevent_polygonsExplosionsRV@data$layer >= 2 & Allevent_polygonsExplosionsRV@data$layer < 4, "4",
                                                            ifelse(Allevent_polygonsExplosionsRV@data$layer >= 4 & Allevent_polygonsExplosionsRV@data$layer < 6, "6", 
                                                                   ifelse (Allevent_polygonsExplosionsRV@data$layer >= 6 & Allevent_polygonsExplosionsRV@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLEDExplosionsRV)
plot(heatALLACLED, add=TRUE)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2ExplosionsRV, add=T)

##Density / Hotspot Analysis: Violence Against Citizens
##Density / Hotspot Analysis
ACLED_ptViolenceCivilians = ACLED_pt[(ACLED_pt$event_type == "Violence against civilians"),]

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_ptViolenceCivilians$PRESENT <- 1

#raster of All ACLED points
rasterACLEDViolenceCivilians <- rasterize(ACLED_ptViolenceCivilians, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2ViolenceCivilians<-log(rasterACLEDViolenceCivilians)

#focal width / developing the kernel
kernelViolenceCivilians = focalWeight(rasterACLED2ViolenceCivilians, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLEDViolenceCivilians = focal(rasterACLED2ViolenceCivilians, kernelViolenceCivilians, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygonsViolenceCivilians = rasterToPolygons(x =heatALLACLEDViolenceCivilians, n=8, dissolve=TRUE)

Allevent_polygons2ViolenceCivilians<-Allevent_polygonsViolenceCivilians

#bin values of the polygons
Allevent_polygons_class2ViolenceCivilians <- Allevent_polygonsViolenceCivilians[Allevent_polygonsViolenceCivilians$layer > 0 & Allevent_polygonsViolenceCivilians$layer < 2,]
Allevent_polygons_class4ViolenceCivilians <- Allevent_polygonsViolenceCivilians[Allevent_polygonsViolenceCivilians$layer >= 2 & Allevent_polygonsViolenceCivilians$layer < 4,]
Allevent_polygons_class6ViolenceCivilians <- Allevent_polygonsViolenceCivilians[Allevent_polygonsViolenceCivilians$layer >= 4 & Allevent_polygonsViolenceCivilians$layer < 6,]
Allevent_polygons_class8ViolenceCivilians <- Allevent_polygonsViolenceCivilians[Allevent_polygonsViolenceCivilians$layer >= 6 & Allevent_polygonsViolenceCivilians$layer < 8,]
Allevent_polygons_classgrt8ViolenceCivilians <- Allevent_polygonsViolenceCivilians[Allevent_polygonsViolenceCivilians$layer >= 8,]

Allevent_polygons2ViolenceCivilians@data$layer <-ifelse (Allevent_polygonsViolenceCivilians@data$layer >= 0 & Allevent_polygonsViolenceCivilians@data$layer < 2, "2",
                                                         ifelse (Allevent_polygonsViolenceCivilians@data$layer >= 2 & Allevent_polygonsViolenceCivilians@data$layer < 4, "4",
                                                                 ifelse(Allevent_polygonsViolenceCivilians@data$layer >= 4 & Allevent_polygonsViolenceCivilians@data$layer < 6, "6", 
                                                                        ifelse (Allevent_polygonsViolenceCivilians@data$layer >= 6 & Allevent_polygonsViolenceCivilians@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLEDViolenceCivilians)
plot(heatALLACLED, add=TRUE)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2ViolenceCivilians, add=T)


##Density / Hotspot Analysis: Riots
##Density / Hotspot Analysis
ACLED_ptRiots = ACLED_pt[(ACLED_pt$event_type == "Riots"),]

#pixel size
pixelsize = .5 

#Africa bounding box / raster template
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info,
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

#field--NEEDS TO BE 1 for the rasterize function in this case since looking for spatial clusters not value clusters
ACLED_ptRiots$PRESENT <- 1

#raster of All ACLED points
rasterACLEDRiots <- rasterize(ACLED_ptRiots, template, field = 'PRESENT', fun = sum)

#log of raster acled point, just for visualization sake
rasterACLED2Riots<-log(rasterACLEDRiots)

#focal width / developing the kernel
kernelRiots = focalWeight(rasterACLED2Riots, d = .01, type = 'Gauss')

#hotspot analysis
heatALLACLEDRiots = focal(rasterACLED2Riots, kernelRiots, fun = sum, na.rm=T)

#hotspot raster to vector
Allevent_polygonsRiots = rasterToPolygons(x =heatALLACLEDRiots, n=8, dissolve=TRUE)

Allevent_polygons2Riots<-Allevent_polygonsRiots

#bin values of the polygons
Allevent_polygons_class2Riots <- Allevent_polygonsRiots[Allevent_polygonsRiots$layer > 0 & Allevent_polygonsRiots$layer < 2,]
Allevent_polygons_class4Riots <- Allevent_polygonsRiots[Allevent_polygonsRiots$layer >= 2 & Allevent_polygonsRiots$layer < 4,]
Allevent_polygons_class6Riots <- Allevent_polygonsRiots[Allevent_polygonsRiots$layer >= 4 & Allevent_polygonsRiots$layer < 6,]
Allevent_polygons_class8Riots <- Allevent_polygonsRiots[Allevent_polygonsRiots$layer >= 6 & Allevent_polygonsRiots$layer < 8,]
Allevent_polygons_classgrt8Riots <- Allevent_polygonsRiots[Allevent_polygonsRiots$layer >= 8,]

Allevent_polygons2Riots@data$layer <-ifelse (Allevent_polygonsRiots@data$layer >= 0 & Allevent_polygonsRiots@data$layer < 2, "2",
                                             ifelse (Allevent_polygonsRiots@data$layer >= 2 & Allevent_polygonsRiots@data$layer < 4, "4",
                                                     ifelse(Allevent_polygonsRiots@data$layer >= 4 & Allevent_polygonsRiots@data$layer < 6, "6", 
                                                            ifelse (Allevent_polygonsRiots@data$layer >= 6 & Allevent_polygonsRiots@data$layer < 8, "8", "9"))))


#plot the polygons
plot(heatALLACLEDRiots)
plot(heatALLACLED, add=TRUE)
##plot(Allevent_polygons, add=T, border='red')
plot(Africa, add=T)
plot(Allevent_polygons2Riots, add=T)


##Leaflet
library(leaflet)

SouthAfrica <- getData("GADM", country="South Africa", level=3)

all <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLED), na.color = "transparent")
battle <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLEDBattle), na.color = "transparent")
protests <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLEDProtests), na.color = "transparent")
explosionsRV <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLEDExplosionsRV), na.color = "transparent")
ViolenceCivilians <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLEDViolenceCivilians), na.color = "transparent")
Riots <- colorNumeric(c("#0000FF", "#00FF00", "#FF0000"), values(heatALLACLEDRiots), na.color = "transparent")

Africa2<-st_as_sf(Africa)
Allevent_polygons22<-st_as_sf(Allevent_polygons2)


finalMap<-leaflet() %>%
        addTiles() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addPolygons(data = Allevent_polygons_class8, color="orange", fillOpacity = 0, group="All-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8, color="red", fillOpacity = 0, group="All-p0.001") %>%
        
        addPolygons(data = Allevent_polygons_class8Battle, color="orange", fillOpacity = 0, group="Battles-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8Battle, color="red", fillOpacity = 0, group="Battles-p0.001") %>%
        
        addPolygons(data = Allevent_polygons_class8Protests, color="orange", fillOpacity = 0, group="Protests-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8Protests, color="red", fillOpacity = 0, group="Protests-p0.001") %>%
        
        addPolygons(data = Allevent_polygons_class8ExplosionsRV, color="orange", fillOpacity = 0, group="Explosions / RV-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8ExplosionsRV, color="red", fillOpacity = 0, group="Explosions / RV-p0.001") %>%
        
        addPolygons(data = Allevent_polygons_class8ViolenceCivilians, color="orange", fillOpacity = 0, group="Violence against Civilians-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8ViolenceCivilians, color="red", fillOpacity = 0, group="Violence against Civilians-p0.001") %>%
        
        addPolygons(data = Allevent_polygons_class8Riots, color="orange", fillOpacity = 0, group="Riots-p0.01") %>%
        addPolygons(data = Allevent_polygons_classgrt8Riots, color="red", fillOpacity = 0, group="Riots-p0.001") %>%
        
        addRasterImage(heatALLACLED, colors = all, opacity = 0.5, group='All') %>%
        addRasterImage(heatALLACLEDBattle, colors = battle, opacity = 0.5, group='Battles') %>%
        addRasterImage(heatALLACLEDProtests, colors = battle, opacity = 0.5, group='Protests') %>%
        addRasterImage(heatALLACLEDExplosionsRV, colors = explosionsRV(), opacity = 0.5, group="Explosions / RV") %>%
        addRasterImage(heatALLACLEDExplosionsRV, colors = ViolenceCivilians, opacity = 0.5, group="Violence against Civilians") %>%
        addRasterImage(heatALLACLEDExplosionsRV, colors = Riots, opacity = 0.5, group="Riots") %>%
        ##addPolygons(data = SouthAfrica, weight = 2, color = "red", fillOpacity = 0, group="South Africa")
        ##addMarkers(ACLED_pt, lng = ~lonitude, lat = ~latitude,color = "red", group="All Conflict Points") %>%
        
        addLegend(pal = pal, values = values(heatALLACLED), title = "Log Density Function", position = "bottomleft")%>%
        
        addMiniMap(position = "bottomright")%>%
        
        addLayersControl(
                baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                overlayGroups = c("All", "Battles", "Protests", "Explosions / RV", "Violence against Civilians", "Riots", "All-p0.01", "All-p0.001", "Battles-p0.01", "Battles-p0.001", "Protests-p0.01", "Protests-p0.001", "Explosions / RV-p0.01", "Explosions / RV-p0.001", "Violence against Civilians-p0.01", "Violence against Civilians-p0.001", "Riots-p0.01", "Riots-p0.001"), options = layersControlOptions(collapsed = T))
        setView(lng = 20, lat = -5, zoom = 3)

finalMap

library(htmlwidgets)

saveWidget(finalMap, file="final_HotspotMap.html")

##Post-Hoc Analysis##
ACLED_pt$hotspot<-extract(heatALLACLED, ACLED_pt)
ALlevent_hotspot_merge<-merge(ACLED_merge, ACLED_pt, by = "data_id") 
