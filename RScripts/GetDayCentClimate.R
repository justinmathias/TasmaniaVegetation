#Created by Justin Mathias, 5/1/2023
#This code will create a weather file for a single site using thredds data portal using John Abatzaglou's data

library('easypackages')
libraries(c('tidyverse','HelpersMG', 'ncdf4', 'lubridate', 'rlist'))

#Define sites
tassites <- data.frame('SiteID' = c('TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
                       'Lat' = c(-41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
                       'Lon' = c(148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781))


# tas <- data.frame('SiteID' = c('TheKeepDry'),
#                   'Lat' = -41.16737,
#                   'Lon' = 148.07137)
# 
# 
# 
# 
# #Define lat/lon for downloading
# latnorth <- tassites$Lat+0.01
# latsouth <- tassites$Lat-0.01
# lonwest <- tassites$Lon-0.01
# loneast <- tassites$Lon+0.01
# 
# 
# #We will give a single lat lon and have the values round to nearest 0.05 for latlow, lathigh, lonlow, lonhigh
# StartYear <- 1910:2020
# EndYear <- StartYear+1

#Create file links and download all files necessary for analysis----
#We need precip (mm), tmax (C), and tmin (C) to create a weather (wth) file for DayCent.
#We could also add srad, rh, and ws are all optional and we will not include them here.

# ##precip----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/precip")
# precip_dls <- list()
# for (i in seq_along(latnorth)) {
#   for (j in seq_along(StartYear))
#   precip_dls <- append(precip_dls,paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/precip/total/r005/01day/agcd_v1_precip_total_r005_daily_",StartYear[j],".nc?var=precip&north=",latnorth[i],"&west=",lonwest[i],"&east=",loneast[i],"&south=",latsouth[i],"&disableProjSubset=on&horizStride=1&time_start=",StartYear[j],"-01-01T09%3A00%3A00Z&time_end=",EndYear[j],"-12-31T09%3A00%3A00Z&timeStride=1&addLatLon=true"))
# }
# precip_dls <- unlist(precip_dls)
# wget(url = c(precip_dls))
# 
# ##tmax----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmax")
# tmax_dls <- list()
# for (i in seq_along(latnorth)) {
#   for (j in seq_along(StartYear))
#     tmax_dls <- append(tmax_dls,paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmax/mean/r005/01day/agcd_v1_tmax_mean_r005_daily_",StartYear[j],".nc?var=tmax&north=",latnorth[i],"&west=",lonwest[i],"&east=",loneast[i],"&south=",latsouth[i],"&disableProjSubset=on&horizStride=1&time_start=",StartYear[j],"-01-01T09%3A00%3A00Z&time_end=",EndYear[j],"-12-31T09%3A00%3A00Z&timeStride=1&addLatLon=true"))
# }
# tmax_dls <- unlist(tmax_dls)
# wget(url = c(tmax_dls))
# 
# ##tmin----
# setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmin")
# tmin_dls <- list()
# for (i in seq_along(latnorth)) {
#   for (j in seq_along(StartYear))
#     tmin_dls <- append(tmin_dls,paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmin/mean/r005/01day/agcd_v1_tmin_mean_r005_daily_",StartYear[j],".nc?var=tmin&north=",latnorth[i],"&west=",lonwest[i],"&east=",loneast[i],"&south=",latsouth[i],"&disableProjSubset=on&horizStride=1&time_start=",StartYear[j],"-01-01T09%3A00%3A00Z&time_end=",EndYear[j],"-12-31T09%3A00%3A00Z&timeStride=1&addLatLon=true"))
# }
# tmin_dls <- unlist(tmin_dls)
# wget(url = c(tmin_dls))

##Read in the list of files downloaded for this analysis----
#There should be 1,110 nc files for each variable, 111 for each of 10 sites
#List files downloaded for analysis
precip_fls <- list.files("/Users/justinmathias/Desktop/TassieDayCent/NC Files/precip/")
tmax_fls <- list.files("/Users/justinmathias/Desktop/TassieDayCent/NC Files/tmax/")
tmin_fls <- list.files("/Users/justinmathias/Desktop/TassieDayCent/NC Files/tmin/")

#Create weather files----
#Preallocate list
precip_out <- list()
tmax_out <- list()
tmin_out <- list()

#Loop through precipitation
for (i in seq_along(precip_fls)) {
  
  #Open ncdf file and get variable, lat, lon, time
  tmp <- nc_open(paste0("/Users/justinmathias/Desktop/TassieDayCent/NC Files/precip/",precip_fls[i]))
  
  lat <- ncvar_get(tmp, "lat")
  lon <- ncvar_get(tmp, "lon")
  time <- ncvar_get(tmp, 'time')
  precip <- ncvar_get(tmp, "precip")
  
  #Create empty data frame for storing variables
  dat <- data.frame(matrix(nrow = length(time), ncol = 6)) %>% rename("day" = "X1", "month" = "X2", "year" = "X3", "jday" = "X4", "precip" = "X5")
  #Create empty list to store data in
  
  
  #Extract day, month, year, and julian day as integers using lubridate.
  #This assumes all files are in standard format of days since 1850-01-01
  for (j in 1:length(time)) {
    dat$day[j] <- day(as_date(-43829 +time[j]))
    dat$month[j] <- month(as_date(-43829 +time[j]))
    dat$year[j] <- year(as_date(-43829 +time[j]))
    dat$jday[j] <- yday(as_date(-43829 +time[j]))
    dat$precip[j] <- ifelse(length(lat) == 1 & length(lon == 2),mean(precip[,j]),
                            ifelse(length(lat) == 2 & length(lon == 2),mean(precip[,,j]),
                                   ifelse(length(lat) == 2 & length(lon == 1),mean(precip[,j]),"NA")))
    
    
  }
  
  precip_out <-  append(precip_out, dat)
  
}

precip_output <- list.rbind(precip_out)








}

precip_out <-  append(precip_out, dat)

}

precip_output <- list.rbind(precip_out)







