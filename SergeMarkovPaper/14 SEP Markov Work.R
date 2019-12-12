remove(list=ls())

library(raster)
library(rgdal)
library(foreign)
library(sp)
library(naniar)
library(dplyr)

#setwd("C:/Users/Jay/Desktop/MARKOV Work") #backup
setwd("C:/Users/Jay/Dropbox/WaterScarcity_NationalSecurity/Herndon")

########################################################################
###Combining two data sets##############################################
########################################################################
load(file="Markov_data.RData")
load(file = "Markov_data_rain.RData")

markov_data$cell <- as.factor(markov_data$cell)
markov_data_rain$V2 <- as.factor(markov_data_rain$V2)
colnames(markov_data_rain) <- c("year","cell","rain")

markov_data_merged <- merge(markov_data,markov_data_rain,by=c("cell","year"))

cells <- unique(markov_data_merged$cell)
years <- unique(markov_data_merged$year)

markov_data_adjusted_cell <- as.data.frame(matrix(0,nrow=0,ncol=22))

for(j in 1:length(cells)){
  #j <- 2
  
  subset_cell <- markov_data_merged[markov_data_merged$cell==cells[j],]
  
  adjusted_cell <- as.data.frame(matrix(0,nrow=length(years),ncol=22))
  adjusted_cell[,1:12] <- subset_cell
  
  for(i in 1:length(years)){
    #i <- 1
    adjusted_cell[i,13] <- adjusted_cell[i,3]-mean(adjusted_cell$V3)
    adjusted_cell[i,14] <- adjusted_cell[i,4]-mean(adjusted_cell$V4)
    adjusted_cell[i,15] <- adjusted_cell[i,5]-mean(adjusted_cell$V5)
    adjusted_cell[i,16] <- adjusted_cell[i,6]-mean(adjusted_cell$V6)
    adjusted_cell[i,17] <- adjusted_cell[i,7]-mean(adjusted_cell$V7)
    adjusted_cell[i,18] <- adjusted_cell[i,8]-mean(adjusted_cell$V8)
    adjusted_cell[i,19] <- adjusted_cell[i,9]-mean(adjusted_cell$V9)
    adjusted_cell[i,20] <- adjusted_cell[i,10]-mean(adjusted_cell$V10)
    adjusted_cell[i,21] <- adjusted_cell[i,11]-mean(adjusted_cell$V11)
    adjusted_cell[i,22] <- adjusted_cell[i,12]-mean(adjusted_cell$V12, na.rm=TRUE)
    
  }
  
  markov_data_adjusted_cell <- rbind(markov_data_adjusted_cell,adjusted_cell)
}
colnames(markov_data_adjusted_cell) <- c("cell","year",
                                         "total","protests","exposions_remote_violence","strategic_developments",
                                         "battles","violence_against_civilians","riots","violent","nonviolent","rain",
                                         "total_mean","protests_mean","exposions_remote_violence_mean","strategic_developments_mean",
                                         "battles_mean","violence_against_civilians_mean","riots_mean","violent_mean","nonviolent_mean","rain_mean")

save(markov_data_adjusted_cell,file="markov_data_adjusted_cell.Rdata")
########################################################################
#Work to generate the rainfall independent varaibles for the ###########
#country/year observations used in Markov Analysis######################
########################################################################


#data
load(file="mean_rainfall_1997_2018.RData") #see "prepping rainfall" below for code on how this was put together

Africa<-readOGR(".", "Africa")
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

pixelsize = .5 
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info, #169 x 154 raster, 26,026 units
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

template_mod <-  as(template, "SpatialPolygonsDataFrame") #template_mod$layer[169] to verfiy that this creates polygons by row

template_mod$cell_number <- 1

for(i in 1:length(template_mod)){
  template_mod$cell_number[i] <- i
}


#These five lines are to verfiy that it works

#cell_polygon <- template_mod[template_mod$cell_number==100,]
#cell_rain <- extract(STACK_rain_mean_1997_2018[[1]],cell_polygon,fun=mean,na.rm=TRUE)
#cell_rain <- as.data.frame(cell_rain)
#cell_rain <- cell_rain[cell_rain>0,]
#cell_mean <- mean(cell_rain)

 
years<-c(1997:2018) #22 years
markov_data_rain <- as.data.frame(matrix(0,nrow=3,ncol=0))

for(ii in 1:length(years)){ #length(years)
  #ii <- 3
  y <- years[ii] 
  year_1 <- as.data.frame(matrix(0,nrow =nrow(template)*ncol(template),ncol=3)) #nrow(template)*ncol(template)
  k <- 1
  for(i in 1:nrow(template)){ #nrow(template)
    #i <- 1
    for(j in 1:ncol(template)){#ncol(template)
      #j <- 1
      year_1[k,1] <- y
      year_1[k,2] <- paste0(i,"/",j)
      cell_polygon <- template_mod[template_mod$cell_number==k,]
      cell_rain <- extract(STACK_rain_mean_1997_2018[[ii]],cell_polygon)
      cell_rain <- as.data.frame(cell_rain)
      cell_rain <- cell_rain[cell_rain>0,]
      cell_mean <- mean(cell_rain)
      year_1[k,3] <- cell_mean
      k <- k+1
    }
  }
  markov_data_rain <- rbind(markov_data_rain,year_1)
  print(years[[ii]])
}

save(markov_data_rain,file = "Markov_data_rain.RData")
write.dta(markov_data_rain, "Markov_data_rain.dta")



########################################################################
#Work to generate the data for Markov analysis##########################
#Conflict count (all ACLED) by cell/year################################
########################################################################

Africa<-readOGR(".", "Africa")
projection_info<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(Africa)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

ACLED_Northern<-read.csv("ACLED9719_Northern_Africa.csv")
ACLED_Eastern<-read.csv("ACLED9719_Eastern_Africa.csv")
ACLED_Central<-read.csv("ACLED9719_Middle_Africa.csv")
ACLED_Western<-read.csv("ACLED9719_Western_Africa.csv")
ACLED_Southern<-read.csv("ACLED9719_Southern_Africa.csv")

ACLED_merge<-rbind(ACLED_Northern, ACLED_Eastern, ACLED_Central, ACLED_Western, ACLED_Southern)
coords <- ACLED_merge[c("longitude","latitude")] 
ACLED_pt <- SpatialPointsDataFrame(coords = coords, ACLED_merge, proj4string = projection_info) 
ACLED_pt$PRESENT <- 1

ACLED_merge_protests <- ACLED_merge[ACLED_merge$event_type=="Protests",]
coords_protest <- ACLED_merge_protests[c("longitude","latitude")] 
ACLED_pt_protests <- SpatialPointsDataFrame(coords = coords_protest, ACLED_merge_protests, proj4string = projection_info) 
ACLED_pt_protests$PRESENT <- 1

ACLED_merge_explosion_remote_violence <- ACLED_merge[ACLED_merge$event_type=="Explosions/Remote violence",]
coords_explosion_remote_violence <- ACLED_merge_explosion_remote_violence[c("longitude","latitude")] 
ACLED_pt_explosion_remote_violence <- SpatialPointsDataFrame(coords = coords_explosion_remote_violence, ACLED_merge_explosion_remote_violence, proj4string = projection_info) 
ACLED_pt_explosion_remote_violence$PRESENT <- 1

ACLED_merge_strategic_developments <- ACLED_merge[ACLED_merge$event_type=="Strategic developments",]
coords_strategic_developments <- ACLED_merge_strategic_developments[c("longitude","latitude")] 
ACLED_pt_strategic_developments <- SpatialPointsDataFrame(coords = coords_strategic_developments, ACLED_merge_strategic_developments, proj4string = projection_info) 
ACLED_pt_strategic_developments$PRESENT <- 1

ACLED_merge_battles <- ACLED_merge[ACLED_merge$event_type=="Battles",]
coords_battles <- ACLED_merge_battles[c("longitude","latitude")] 
ACLED_pt_battles <- SpatialPointsDataFrame(coords = coords_battles, ACLED_merge_battles, proj4string = projection_info) 
ACLED_pt_battles$PRESENT <- 1

ACLED_merge_violence_against_civilians <- ACLED_merge[ACLED_merge$event_type=="Violence against civilians",]
coords_violence_against_civilians <- ACLED_merge_violence_against_civilians[c("longitude","latitude")] 
ACLED_pt_violence_against_civilians <- SpatialPointsDataFrame(coords = coords_violence_against_civilians, ACLED_merge_violence_against_civilians, proj4string = projection_info) 
ACLED_pt_violence_against_civilians$PRESENT <- 1

ACLED_merge_riots <- ACLED_merge[ACLED_merge$event_type=="Riots",]
coords_riots <- ACLED_merge_riots[c("longitude","latitude")] 
ACLED_pt_riots <- SpatialPointsDataFrame(coords = coords_riots, ACLED_merge_riots, proj4string = projection_info) 
ACLED_pt_riots$PRESENT <- 1

ACLED_merge_violent <- ACLED_merge[ACLED_merge$event_type!="Protests" & ACLED_merge$event_type!="Strategic developments" ,]
coords_violent <- ACLED_merge_violent[c("longitude","latitude")] 
ACLED_pt_violent <- SpatialPointsDataFrame(coords = coords_violent, ACLED_merge_violent, proj4string = projection_info) 
ACLED_pt_violent$PRESENT <- 1

ACLED_merge_nonviolent <- ACLED_merge[ACLED_merge$event_type=="Protests" |ACLED_merge$event_type=="Strategic developments" ,]
coords_nonviolent <- ACLED_merge_nonviolent[c("longitude","latitude")] 
ACLED_pt_nonviolent <- SpatialPointsDataFrame(coords = coords_nonviolent, ACLED_merge_nonviolent, proj4string = projection_info) 
ACLED_pt_nonviolent$PRESENT <- 1


pixelsize = .5 
box = round(extent(Africa) / pixelsize) * pixelsize 
template = raster(box, crs = projection_info, #169 x 154 raster, 26,026 units
                  nrows = (box@ymax - box@ymin) / pixelsize, 
                  ncols = (box@xmax - box@xmin) / pixelsize)

years<-c(1997:2018)

markov_data <- as.data.frame(matrix(0,nrow=0,ncol=11))


for(ii in 1:length(years)){#length(years)
  y <- years[ii] 
  
  ACLED<-ACLED_pt[ which(ACLED_pt$year==y),]
  rasterACLED <- rasterize(ACLED, template, field = 'PRESENT', fun = sum) #169 x 154 raster
  rasterACLED[is.na(rasterACLED[])] <- 0 
  
  ACLED_protests <-ACLED_pt_protests[ which(ACLED_pt_protests$year==y),]
  rasterACLED_protests <- rasterize(ACLED_protests, template, field = 'PRESENT', fun = sum) 
  rasterACLED_protests[is.na(rasterACLED_protests[])] <- 0 
  
  ACLED_explosion_remote_violence <-ACLED_pt_explosion_remote_violence[ which(ACLED_pt_explosion_remote_violence$year==y),]
  rasterACLED_explosion_remote_violence <- rasterize(ACLED_explosion_remote_violence, template, field = 'PRESENT', fun = sum) 
  rasterACLED_explosion_remote_violence[is.na(rasterACLED_explosion_remote_violence[])] <- 0
  
  ACLED_strategic_developments <-ACLED_pt_strategic_developments[ which(ACLED_pt_strategic_developments$year==y),]
  rasterACLED_strategic_developments <- rasterize(ACLED_strategic_developments, template, field = 'PRESENT', fun = sum) 
  rasterACLED_strategic_developments[is.na(rasterACLED_strategic_developments[])] <- 0 
  
  ACLED_battles <-ACLED_pt_battles[ which(ACLED_pt_battles$year==y),]
  rasterACLED_battles <- rasterize(ACLED_battles, template, field = 'PRESENT', fun = sum) 
  rasterACLED_battles[is.na(rasterACLED_battles[])] <- 0 
  
  ACLED_violence_against_civilians <-ACLED_pt_violence_against_civilians[ which(ACLED_pt_violence_against_civilians$year==y),]
  rasterACLED_violence_against_civilians <- rasterize(ACLED_violence_against_civilians, template, field = 'PRESENT', fun = sum) 
  rasterACLED_violence_against_civilians[is.na(rasterACLED_violence_against_civilians[])] <- 0
  
  ACLED_riots <-ACLED_pt_riots[ which(ACLED_pt_riots$year==y),]
  rasterACLED_riots <- rasterize(ACLED_riots, template, field = 'PRESENT', fun = sum) 
  rasterACLED_riots[is.na(rasterACLED_riots[])] <- 0
  
  ACLED_violent <-ACLED_pt_violent[ which(ACLED_pt_violent$year==y),]
  rasterACLED_violent <- rasterize(ACLED_violent, template, field = 'PRESENT', fun = sum) 
  rasterACLED_violent[is.na(rasterACLED_violent[])] <- 0
  
  ACLED_nonviolent <-ACLED_pt_nonviolent[ which(ACLED_pt_nonviolent$year==y),]
  rasterACLED_nonviolent <- rasterize(ACLED_nonviolent, template, field = 'PRESENT', fun = sum) 
  rasterACLED_nonviolent[is.na(rasterACLED_nonviolent[])] <- 0
  
  year_1 <- as.data.frame(matrix(0,nrow =nrow(rasterACLED)*ncol(rasterACLED) ,ncol=11))#nrow(rasterACLED)*ncol(rasterACLED)
  
  k <- 1
  for(i in 1:nrow(rasterACLED)){#nrow(rasterACLED)
    for(j in 1:ncol(rasterACLED)){#ncol(rasterACLED)
      year_1[k,1] <- y
      year_1[k,2] <- paste0(i,"/",j)
      year_1[k,3] <- rasterACLED[i,j]
      year_1[k,4] <- rasterACLED_protests[i,j]
      year_1[k,5] <- rasterACLED_explosion_remote_violence[i,j]
      year_1[k,6] <- rasterACLED_strategic_developments[i,j]
      year_1[k,7] <- rasterACLED_battles[i,j]
      year_1[k,8] <- rasterACLED_violence_against_civilians[i,j]
      year_1[k,9] <- rasterACLED_riots[i,j]
      year_1[k,10] <- rasterACLED_violent[i,j]
      year_1[k,11] <- rasterACLED_nonviolent[i,j]
      k <- k+1
    }
  }
  markov_data <- rbind(markov_data,year_1)
  print(years[[ii]])
}

colnames(markov_data) <- c("year","cell","ACLED_total","Protests","Explosions_Remote_Violence",
                           "Strategic_Developments","Battles","Violence_against_civilians","Riots",
                           "Violent","Nonviolent")

save(markov_data,file = "Markov_data.RData")
write.dta(markov_data, "Markov_data.dta")



#######################################################################################################################
#prepping rainfall (had to update becuase previous rainfall work only went up to 2017 to match W&S temp data)##########
#######################################################################################################################

setwd("C:/Users/Jay/Desktop/SESYNC/21 AUG CHIRPS")

years <- 1997:2018 #these are the years that match the temp data
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

for(j in 1:length(years)){
  
  f<- list()
  for(i in 1:length(months)){
    f[i] <- paste0("chirps-v2.0.",years[j],".",months[i],".tif" )
  }
  
  ras <- lapply(f,raster) 
  STACK1 <- stack(ras) 
  
  mean_rainfall_raster <- calc(STACK1, fun = mean)
  f <- paste0('mean_rain_', years[j], '.tif')
  writeRaster(mean_rainfall_raster, filename=f,overwrite=TRUE)
  
  
}


f<- list()
for(i in 1:length(years)){#length(years)
  f[i] <- paste0('mean_rain_', years[i], '.tif')
}

ras <- lapply(f,raster) 
STACK_rain_mean_1997_2018 <- stack(ras) 

save(STACK_rain_mean_1997_2018,file="mean_rainfall_1997_2018.RData")


summary(STACK_rain_mean_1997_2018)
