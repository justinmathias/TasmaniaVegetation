#Created by Justin Mathias 3/12/23

library('easypackages')
libraries(c('tidyverse','HelpersMG', 'ncdf4'))

#Define sites across northern Tasmania
tassites <- data.frame('SiteID' = c('TheKeepDry','TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
                       'Latitude' = c(-41.16737, -41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
                       'Longitude' = c(148.07137, 148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781))

#List example URL for precip total, tmax mean, and tmin mean
#The following is the base URL for 2020 for precip total:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/precip/total/r005/01day/agcd_v1_precip_total_r005_daily_2020.nc?var=precip&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true
#The following is the base URL for 2020 for tmax mean:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmax/mean/r005/01day/agcd_v1_tmax_mean_r005_daily_2020.nc?var=tmax&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true
#The following is the base URL for 2020 for tmin mean:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmin/mean/r005/01day/agcd_v1_tmin_mean_r005_daily_2020.nc?var=tmin&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true

#Define boundary points used to get NCDF files of climate data for each of our sites from thredds server--we need a rectangle as we cannot extract single points.
latnorth <- tassites$Latitude+0.01
latsouth <- tassites$Latitude-0.01
lonwest <- tassites$Longitude+0.01
loneast <- tassites$Longitude-0.01

#Define years over which we want to extract climate data
yrs <- 1900:1901

#Define URLs for precip data products (NCDF files) to download from thredds server
precip.files <- vector("list", length(yrs)*length(latnorth)) #Create empty list to store filenames in 
for (i in 1:length(yrs)) {
  for (j in 1:length(tassites$SiteID)) {
  precip.files[[i]][j] <- paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/precip/total/r005/01day/agcd_v1_precip_total_r005_daily_",yrs[i],".nc?var=precip&north=",latnorth[j],"&west=",lonwest[j],"&east=",loneast[j],"&south=",latsouth[j],"&horizStride=1&time_start=",yrs[i],"-01-01T09%3A00%3A00Z&time_end=",yrs[i],"-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
  }
}
ppt.urls <- unlist(precip.files)

#Define URLs for tmax data products (NCDF files) to download from thredds server
tmax.files <- vector("list", length(yrs)*length(latnorth)) #Create empty list to store filenames in 
for (i in 1:length(yrs)) {
  for (j in 1:length(tassites$SiteID)) {
    tmax.files[[i]][j] <- paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmax/mean/r005/01day/agcd_v1_tmax_mean_r005_daily_",yrs[i],".nc?var=tmax&north=",latnorth[j],"&west=",lonwest[j],"&east=",loneast[j],"&south=",latsouth[j],"&horizStride=1&time_start=",yrs[i],"-01-01T09%3A00%3A00Z&time_end=",yrs[i],"-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
  }
}
tmax.urls <- unlist(tmax.files)

#Define URLs for tmin data products (NCDF files) to download from thredds server
tmin.files <- vector("list", length(yrs)*length(latnorth)) #Create empty list to store filenames in 
for (i in 1:length(yrs)) {
  for (j in 1:length(tassites$SiteID)) {
    tmin.files[[i]][j] <- paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmin/mean/r005/01day/agcd_v1_tmin_mean_r005_daily_",yrs[i],".nc?var=tmin&north=",latnorth[j],"&west=",lonwest[j],"&east=",loneast[j],"&south=",latsouth[j],"&horizStride=1&time_start=",yrs[i],"-01-01T09%3A00%3A00Z&time_end=",yrs[i],"-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
  }
}
tmin.urls <- unlist(tmin.files)



destpath <- "/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/Daily/"

var <- "precip"
for (i in 1:length(ppt.urls)) {
  download.file(url = ppt.urls[i], destfile = paste0(destpath), method = "wget")
}

download.file('https://www.wikipedia.org/', destfile = "mydirectory/wikipage.html", method = "wget", extra = "-r -p --random-wait")

download.file(url = ppt.urls[1], destfile = paste0(destpath, "file.nc", method = "wget"))






#I owe Livi 39.50.





tmp <- nc_open("/Users/justinmathias/Desktop/untitled folder/agcd_v1_precip_total_r005_daily_1900.nc?var=precip&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=1900-01-01T09%3A00%3A00Z&time_end=1900-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
tmp
ncvar_get(tmp, "precip")
