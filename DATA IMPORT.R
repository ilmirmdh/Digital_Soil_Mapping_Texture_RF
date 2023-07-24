##----Load Packages-----
library(dplyr)
library(stats)
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(caret)

#----Load Shapefile Data----
# Soil Data
shpLAB <- st_read("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\POINT\\Survey_Test.shp")
colnames(shpLAB)
skw <- st_read("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\ARCMAP\\AREA PENELITIAN\\SHAPEFILE\\TEA_DISSOLVED.shp")

# Geology
shpGEO <- readOGR("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\ARCMAP\\SHAPEFILE\\Lith.shp")
shpGEO@data$Formations <- as.factor(shpGEO@data$Formations)
symbolgeo.lev <- levels(shpGEO@data$Formations)
colnames(shpGEO)
# Soil Type
shpSOIL <- readOGR("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\ARCMAP\\SHAPEFILE\\Tanah.shp")
shpSOIL@data$Tanah <- as.factor(shpSOIL@data$Tanah)
symbolsoil.lev <- levels(shpSOIL$Tanah)
colnames(shpSOIL)
# Geomorphon
shpGMR <- readOGR("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\ARCMAP\\SHAPEFILE\\Geomorphons.shp")
shpGMR@data$geomorph <- as.factor(shpGMR@data$geomorph)
symbolgmr.lev <- levels(shpGMR$geomorph)

#----Load Covariate Rasters
# Organism Covariates
r.avi <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\ORGANISM\\AVI.tif")
names(r.avi) <- "AVI"
r.avi <- crop(r.avi, extent(skw))
r.avi <- mask(r.avi, skw)
r.bri <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\ORGANISM\\BRI.tif")
names(r.bri) <- "BRI"
r.bri <- crop(r.bri, extent(skw))
r.bri <- mask(r.bri, skw)
r.dwsi <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\ORGANISM\\DWSI.tif")
names(r.dwsi) <- "DWSI"
r.dwsi <- crop(r.dwsi, extent(skw))
r.dwsi <- mask(r.dwsi, skw)
r.ndvi <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\ORGANISM\\NDVI.tif")
names(r.ndvi) <- "NDVI"
r.ndvi <- crop(r.ndvi, extent(skw))
r.ndvi <- mask(r.ndvi, skw)

# Parent Material Covariates
r.gos <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Gossan.tif")
names(r.gos) <- "Gossan"
r.gos <- crop(r.gos, extent(skw))
r.gos <- mask(r.gos, skw)
r.ill <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Illite.tif")
names(r.ill) <- "Illite"
r.ill <- crop(r.ill, extent(skw))
r.ill <- mask(r.ill, skw)
r.kao <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Kaolinite.tif")
names(r.kao) <- "Kaolinite"
r.kao <- crop(r.kao, extent(skw))
r.kao <- mask(r.kao, skw)
r.lat <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Laterite.tif")
names(r.lat) <- "Laterite"
r.lat <- crop(r.lat, extent(skw))
r.lat <- mask(r.lat, skw)
r.mus <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Muscovite.tif")
names(r.mus) <- "Muscovite"
r.mus <- crop(r.mus, extent(skw))
r.mus <- mask(r.mus, skw)
r.phen <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\HYPERION\\COVARIATES\\PARENT\\Phengitic.tif")
names(r.phen) <- "Phengitic"
r.phen <- crop(r.phen, extent(skw))
r.phen <- mask(r.phen, skw)


# Topography Covariates
r.elev <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\DEM_RESAMPLED.tif")
names(r.elev) <- "ELEV"
r.elev <- crop(r.elev, extent(skw))
r.elev <- mask(r.elev, skw)
r.slope <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Slope.tif")
names(r.slope) <- "SLOPE"
r.slope <- crop(r.slope, extent(skw))
r.slope <- mask(r.slope, skw)
r.tri <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Terrain Ruggedness Index (TRI).tif")
names(r.tri) <- "TRI"
r.tri <- crop(r.tri, extent(skw))
r.tri <- mask(r.tri, skw)
r.vdepth <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Valley Depth.tif")
names(r.vdepth) <- "VDEPTH"
r.vdepth <- crop(r.vdepth, extent(skw))
r.vdepth <- mask(r.vdepth, skw)
r.tpi <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Topographic Position Index.tif")
names(r.tpi) <- "TPI"
r.tpi <- crop(r.tpi, extent(skw))
r.tpi <- mask(r.tpi, skw)
r.tex <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Texture.tif")
names(r.tex) <- "TEXT"
r.tex <- crop(r.tex, extent(skw))
r.tex <- mask(r.tex, skw)
r.grad <- raster("C:\\Users\\ilmir\\OneDrive\\Documents\\THESIS\\DATA\\DEM\\Gradient.tif")
names(r.grad) <- "GRAD"
r.grad <- crop(r.grad, extent(skw))
r.grad <- mask(r.grad, skw)

#----Shapefile Rasterization----
r.geomap <- rasterize(x = shpGEO, y = r.ndvi, field = "Formations")
r.soilmap <- rasterize(x = shpSOIL, y = r.ndvi, field = "Tanah")
r.gmr <- rasterize(x = shpGMR, y = r.ndvi, field = "geomorph")

#----Renaming Code Rasterized Shapefile----
names(r.geomap) <- "Lith"
names(r.soilmap) <- "Soil"
names(r.gmr) <- "Geomorph"
r.gmr <- crop(r.gmr, extent(ckp))
r.gmr <- mask(r.gmr, ckp)
print(r.gmr)

#----Stacking All Raster Datas----
raster.stack <- stack (r.avi, r.bri, r.dwsi, r.ndvi, r.gos, r.ill, r.kao, r.lat,
                       r.mus, r.phen, r.geomap, r.elev, r.slope, r.tri, r.vdepth,
                       r.tpi, r.tex, r.grad, r.soilmap, r.gmr)
raster.stack <- stack (r.tex, r.ill, r.phen, r.dwsi, r.ndvi, r.elev, r.lat, r.mus,
                       r.gos, r.avi, r.bri)
raster.stack <- stack (r.ndvi, r.dwsi, r.ill, r.lat, r.gos)
raster.stack <- crop(raster.stack, extent(ckp))
raster.stack <- mask(raster.stack, ckp)

#----Extracting Values From Rasters to Sample's Shapefile
rasvalue <- raster::extract(raster.stack, shpLAB, sp = 1, method = "simple")
rasvalue <- as.data.frame(rasvalue)
rasvalue <- rasvalue[,-c(1,2,3,4,5,27,28)]
rasvalue <- rasvalue[,c("AVI", "BRI", "DWSI", "NDVI", "Gossan", 
                        "Illite", "Kaolinite", "Laterite", "Muscovite", 
                        "Phengitic", "Lith", "ELEV", "SLOPE", "TRI", "VDEPTH",
                        "TPI", "TEXT", "GRAD", "Soil", "Geomorph", "Liat")]
rasvalue <- rasvalue[,c("NDVI", "DWSI", "Illite", "Laterite", "Gossan", 
                        "Phengitic", "Soil", "Muscovite","BRI", "AVI", 
                        "Kaolinite", "Pasir")]
rasvalue <- rasvalue[,c("NDVI", "DWSI", "Illite", "Laterite", "Gossan", "Pasir")]
rasvalue <- rasvalue[,c(Predictors, "Liat")]
colnames(rasvalue)

library(boot)
library(MASS)
library(rcompanion)
plotNormalHistogram(rasvalue$Kaolinite)
plotNormalHistogram(SPECTRAL_FIX_BARE$LST)
library(nortest)
ks.test(rasvalue$AVI, "pnorm")
ks.test(T28$NDVI, "pnorm")
