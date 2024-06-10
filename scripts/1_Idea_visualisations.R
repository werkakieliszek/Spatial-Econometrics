# install the packages that will be required
# install.packages("rgdal")
# install.packages("sp")
# install.packages("sf")

# set working directory - the same where we unpacked the downloaded files
setwd("/Users/weronikakieliszek/Desktop/Uni/Masters/3rd Semester/Ekonometria Przestrzenna/Dane")

# clear the workspace, plots and console
rm(list = ls())
if(!is.null(dev.list())) dev.off()
cat("\014") 

# packages for i.a. working with maps and visualising data on maps
#library(rgdal)
library(sp)
library(sf)

# import map 1 - level of poviats, correct projection (source: dane.gov.pl)
mapa <- read_sf(".", "powiaty")

dane <- read.csv("LUDN_3428_CTAB_20240208163006.csv", header = TRUE, sep = ";", dec = ",")

#Put the spatial and economic databases together, remove the partial databases
dane$Kod <- dane$Kod/1000
mapa$Kod <- as.numeric(as.character(mapa$jpt_kod_je))
spatial_data <- merge(y = dane, x = mapa, by.y = "Kod", by.x = "Kod")
rm(mapa)
rm(dane)

beige_area <- rgb(254, 250, 224, 80, names = NULL, maxColorValue = 255)
green_area <- rgb(40, 54, 24, 80, names = NULL, maxColorValue = 255)

pal <- colorRampPalette(c(beige_area, green_area), bias = 1)

plot(spatial_data[,"urodzenia.żywe.na.1000.ludności.2022...."], 
     key.pos = 4, 
     pal = pal,
     main = "Live birth per 1000 capita | 2022")

output_file_path <- "live_birth_2022.png"
png(filename = output_file_path)
plot(spatial_data[,"urodzenia.żywe.na.1000.ludności.2022...."], 
     key.pos = 4, 
     pal = pal,
     main = "Live birth per 1000 capita | 2022")
dev.off()