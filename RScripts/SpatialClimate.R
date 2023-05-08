#Created by Justin Mathias, 5/5/2023
#This script will download and create a map of changes in precip, tmax, and tmin over Tasmania.

library('easypackages')
libraries(c('tidyverse', 'ncdf4', 'lubridate', 'rlist','data.table','stringi', 'HelpersMG','terra')) 

#Define sites
tassites <- data.frame('SiteID' = c('TheKeepDry', 'TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
                       'Lat' = c(-41.16737, -41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
                       'Lon' = c(148.07137, 148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781),
                       'Type' = c('Dry', 'Wet', 'Dry', 'Wet', 'Dry','Wet','Wet','Wet','Wet','Wet'))


# #Create file links and download all files necessary for analysis----
# year <- 1910:2020
# ##precip----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip")
# precip_dls <- list()
# for (i in seq_along(year)) {
#   precip_dls <- append(precip_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/precip/total/r005/01month/agcd_v1_precip_total_r005_monthly_",year[i],".nc"))
# }
# precip_dls <- unlist(precip_dls)
# wget(url = c(precip_dls))
# 
# ##tmax----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmax")
# tmax_dls <- list()
# for (i in seq_along(year)) {
#   tmax_dls <- append(tmax_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/tmax/mean/r005/01month/agcd_v1_tmax_mean_r005_monthly_",year[i],".nc"))
# }
# tmax_dls <- unlist(tmax_dls)
# wget(url = c(tmax_dls))
# 
# ##tmin----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmin")
# tmin_dls <- list()
# for (i in seq_along(year)) {
#   tmin_dls <- append(tmin_dls,paste0("https://dapds00.nci.org.au/thredds/fileServer/zv2/agcd/v1/tmin/mean/r005/01month/agcd_v1_tmin_mean_r005_monthly_",year[i],".nc"))
# }
# tmin_dls <- unlist(tmin_dls)
# wget(url = c(tmin_dls))

##Read in the list of files downloaded for this analysis----
#There should be 1,110 nc files for each variable, 111 for each of 10 sites
#List files downloaded for analysis
precip_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/")
tmax_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmax/")
tmin_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/tmin/")

##Load Tasmania shapefile to mask rasters----
AUS <- vect("/Users/justinmathias/Downloads/STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")
tassie <- subset(AUS, AUS$STE_NAME21 == "Tasmania")

#Load nc file and wrangle to crop remaining files to
nc <- nc_open("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/agcd_v1_precip_total_r005_monthly_1910.nc")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")
tmp <- flip(rast("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/agcd_v1_precip_total_r005_monthly_1910.nc"))
ext(tmp) <- c(min(lon), max(lon), min(lat), max(lat))
crp_tmplt <- terra::crop(tmp[[1]], ext(144, 149, -44, -40.3))
crp_tmplt <- mask(crp_tmplt, tassie)

#Process rasters----
##Start with precip----
precip_flpth <- "/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/Monthly/precip/"

#Create empty raster stack for precip values to go into and resample to make sure spatial attributes/dimensions match
precip_stack <- rast(nrows = 74, ncols = 100,
                     xmin = 144.0138, xmax = 149.0082, ymin = -44.00072, ymax = -40.30608)

precip_stack <- terra::resample(precip_stack, crp_tmplt)

#Create a raster stack of all NC files
for (i in seq_along(precip_fls)) {
  
  precip <- flip(rast(paste0(flpth, precip_fls[i])))
  ext(precip) <- c(min(lon), max(lon), min(lat), max(lat))
  
  #Crop precip raster to tassie
  precip_c <- terra::crop(precip, ext(144, 149, -44, -40.3))
  #Mask precip raster to tassie
  precip_cm <- mask(precip_c, tassie)
  
  for (j in seq_along(1:12)) {
    precip_stack <- append(precip_stack, precip_cm[[j]])
  }
  
}

#Create index to know which layer corresponds to which year
index = data.frame("Year" = rep(1910:2020, each = 12),
"Month" = rep(1:12, times = length(1910:2020)),
"Index" = 1:(length(1910:2020)*length(1:12)))




