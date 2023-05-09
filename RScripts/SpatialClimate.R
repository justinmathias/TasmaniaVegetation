#Created by Justin Mathias, 5/5/2023
#This script will download and create a map of changes in precip, tmax, and tmin over Tasmania.

library('easypackages')
libraries(c('tidyverse', 'ncdf4', 'lubridate', 'rlist','data.table','stringi', 'HelpersMG','terra')) 

#Define sites
tassites <- data.frame('SiteID' = c('TheKeepDry', 'TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
                       'Lat' = c(-41.16737, -41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
                       'Lon' = c(148.07137, 148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781),
                       'Type' = c('Dry', 'Wet', 'Dry', 'Wet', 'Dry','Wet','Wet','Wet','Wet','Wet'))


#Create file links and download all files necessary for analysis----
year <- 1910:2020
##precip----
setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip")
precip_dls <- list()
for (i in seq_along(year)) {
  precip_dls <- append(precip_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/precip/total/r005/01month/agcd_v1_precip_total_r005_monthly_",year[i],".nc"))
}
precip_dls <- unlist(precip_dls)
wget(url = c(precip_dls))

##tmax----
setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmax")
tmax_dls <- list()
for (i in seq_along(year)) {
  tmax_dls <- append(tmax_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/tmax/mean/r005/01month/agcd_v1_tmax_mean_r005_monthly_",year[i],".nc"))
}
tmax_dls <- unlist(tmax_dls)
wget(url = c(tmax_dls))

##tmin----
setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmin")
tmin_dls <- list()
for (i in seq_along(year)) {
  tmin_dls <- append(tmin_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/tmin/mean/r005/01month/agcd_v1_tmin_mean_r005_monthly_",year[i],".nc"))
}
tmin_dls <- unlist(tmin_dls)
wget(url = c(tmin_dls))

##Read in the list of files downloaded for this analysis----
#There should be 1,110 nc files for each variable, 111 for each of 10 sites
#List files downloaded for analysis
precip_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/")
tmax_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmax/")
tmin_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmin/")

##Load Tasmania shapefile to mask rasters----
AUS <- vect("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/AustraliaBoundaries/STE_2021_AUST_SHP_GDA2020")
tassie <- subset(AUS, AUS$STE_NAME21 == "Tasmania")

#Create index to know which layer corresponds to which year
index <- data.frame("Year" = rep(1910:2019, each = 12),
                   "Month" = rep(1:12, times = length(1910:2019)),
                   "Index" = 1:(length(1910:2019)*length(1:12)))

#Load nc file and wrangle to crop remaining files to
nc <- nc_open("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/agcd_v1_precip_total_r005_monthly_1910.nc")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
tmp <- flip(rast("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/agcd_v1_precip_total_r005_monthly_1910.nc"))
ext(tmp) <- c(min(lon), max(lon), min(lat), max(lat))
crp_tmplt <- terra::crop(tmp[[1]], ext(144, 149, -44, -40.3))
# crp_tmplt <- mask(crp_tmplt, tassie)

#Process rasters----
##Start with precip----
precip_flpth <- "/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/"

#Create empty raster stack for precip values to go into and resample to make sure spatial attributes/dimensions match
precip_stack <- rast(nrows = 74, ncols = 100,
                     xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)

precip_stack <- terra::resample(precip_stack, crp_tmplt)
precip_stack

#Create a raster stack of all NC files
for (i in seq_along(precip_fls)) {
  
  precip <- flip(rast(paste0(precip_flpth, precip_fls[i])))
  ext(precip) <- c(min(lon), max(lon), min(lat), max(lat))
  
  #Crop precip raster to tassie
  precip_crop <- terra::crop(precip, ext(144, 149, -44, -40.3))
  #Mask precip raster to tassie
  # precip_cropmask <- mask(precip_crop, tassie)
  
  for (j in seq_along(1:12)) {
    # precip_stack <- append(precip_stack, precip_cropmask[[j]])
    precip_stack <- append(precip_stack, precip_crop[[j]])
    
  }
  
}


##Now do tmax----
tmax_flpth <- "/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmax/"

#Create empty raster stack for tmax values to go into and resample to make sure spatial attributes/dimensions match
tmax_stack <- rast(nrows = 74, ncols = 100,
                     xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)

tmax_stack <- terra::resample(tmax_stack, crp_tmplt)
tmax_stack
#Create a raster stack of all NC files
for (i in seq_along(tmax_fls)) {
  
  tmax <- flip(rast(paste0(tmax_flpth, tmax_fls[i])))
  ext(tmax) <- c(min(lon), max(lon), min(lat), max(lat))
  
  #Crop tmax raster to tassie
  tmax_crop <- terra::crop(tmax, ext(144, 149, -44, -40.3))
  #Mask tmax raster to tassie
  tmax_cropmask <- mask(tmax_crop, tassie)
  
  for (j in seq_along(1:12)) {
    tmax_stack <- append(tmax_stack, tmax_cropmask[[j]])
  }
  
}


##Now do tmin----
tmin_flpth <- "/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmin/"

#Create empty raster stack for tmin values to go into and resample to make sure spatial attributes/dimensions match
tmin_stack <- rast(nrows = 74, ncols = 100,
                   xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)

tmin_stack <- terra::resample(tmin_stack, crp_tmplt)
tmin_stack
#Create a raster stack of all NC files
for (i in seq_along(tmin_fls)) {
  
  tmin <- flip(rast(paste0(tmin_flpth, tmin_fls[i])))
  ext(tmin) <- c(min(lon), max(lon), min(lat), max(lat))
  
  #Crop tmin raster to tassie
  tmin_crop <- terra::crop(tmin, ext(144, 149, -44, -40.3))
  #Mask tmin raster to tassie
  tmin_cropmask <- mask(tmin_crop, tassie)
  
  for (j in seq_along(1:12)) {
    tmin_stack <- append(tmin_stack, tmin_cropmask[[j]])
  }
  
}


#Raster math!----
#First, create a growing season index for Tasmania
grw_index <- index %>% filter(Month %in% c(11,12,1,2,3)) %>% dplyr::select(Index)
#Pull out layers that represent growing season
precip_grw <- precip_stack[[grw_index]]
tmax_grw <- tmax_stack[[grw_index]]
tmin_grw <- tmin_stack[[grw_index]]


#Get precip growing season
#Calculate sum of precip (mean for tmax and tmin) since 1911
precip_grw_trunc <- precip_grw[[4:548]] #truncate to 1911-2019 to make indexing easier
precip_grw_out <- rast(nrows = 74, ncols = 100,
                       xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)
precip_grw_out <- resample(precip_grw_out, precip_grw_trunc)
#Loop over growing season rasters to create sum of growing season ppt for tassie
for (i in 1:109) {
  
  startlayer <- (i-1)*5+1 #this will index over growing season months specified here ;)
  endlayer <- i*5
  
  #Years 1911 - 2019
  precip_grw_out <- append(precip_grw_out, app(precip_grw_trunc[[startlayer:endlayer]], sum))
}


#Get tmax growing season
#Calculate sum of tmax (mean for tmax and tmin) since 1911
tmax_grw_trunc <- tmax_grw[[4:548]] #truncate to 1911-2019 to make indexing easier
tmax_grw_out <- rast(nrows = 74, ncols = 100,
                       xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)
tmax_grw_out <- resample(tmax_grw_out, tmax_grw_trunc)
#Loop over growing season rasters to create sum of growing season ppt for tassie
for (i in 1:109) {
  
  startlayer <- (i-1)*5+1 #this will index over growing season months specified here ;)
  endlayer <- i*5
  
  #Years 1911 - 2019
  tmax_grw_out <- append(tmax_grw_out, app(tmax_grw_trunc[[startlayer:endlayer]], sum))
}



#Get tmin growing season
#Calculate sum of tmin (mean for tmax and tmin) since 1911
tmin_grw_trunc <- tmin_grw[[4:548]] #truncate to 1911-2019 to make indexing easier
tmin_grw_out <- rast(nrows = 74, ncols = 100,
                       xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)
tmin_grw_out <- resample(tmin_grw_out, tmin_grw_trunc)
#Loop over growing season rasters to create sum of growing season ppt for tassie
for (i in 1:109) {
  
  startlayer <- (i-1)*5+1 #this will index over growing season months specified here ;)
  endlayer <- i*5
  
  #Years 1911 - 2019
  tmin_grw_out <- append(tmin_grw_out, app(tmin_grw_trunc[[startlayer:endlayer]], sum))
}


#Get trend for each pixel over time----
#Write function to get slope or pvalue
app_lm <- function(ras, return = "slope") {
  time <- 1:nlyr(ras)
  slopefun <- function(x) { lm(x ~ time)$coefficients[2] }
  pfun <- function(x) { summary(lm(x ~ time))$coefficients[2,4]}
  ras_slope <- app(ras, slopefun)
  ras_p <- app(ras, pfun)
  out <- switch (return,
    "slope" = ras_slope,
    "p" = ras_p
  )
  out
}


app_lm()



time <- 1:nlyr(precip_grw_out)
slopefun <- function(x) { lm(x ~ time)$coefficients[2] }
pfun <- function(x) { summary(lm(x ~ time))$coefficients[2,4]}
precip_slope <- app(precip_grw_out, slopefun)
precip_p <- app(precip_grw_out, pfun)

plot(precip_p)
clamp
slickrick <- ifel(precip_p < 0.05, precip_slope, NA)
plot(mask(slickrick, tassie)*10)
plot(tassie, add = T)




