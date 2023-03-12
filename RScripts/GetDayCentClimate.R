#Created by Justin Mathias 3/12/

library('easypackages')
libraries(c('tidyverse','HelpersMG', 'ncdf4'))

#Define sites
tassites <- data.frame('SiteID' = c('TheKeepDry','TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
                       'Latitude' = c(-41.16737, -41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
                       'Longitude' = c(148.07137, 148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781))


#The following is the base URL for 2020 for precip total:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/precip/total/r005/01day/agcd_v1_precip_total_r005_daily_2020.nc?var=precip&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true

#The following is the base URL for 2020 for tmax mean:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmax/mean/r005/01day/agcd_v1_tmax_mean_r005_daily_2020.nc?var=tmax&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true

#The following is the base URL for 2020 for tmin mean:
#https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/tmin/mean/r005/01day/agcd_v1_tmin_mean_r005_daily_2020.nc?var=tmin&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=2020-01-01T09%3A00%3A00Z&time_end=2020-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true





latnorth <- tassites$Latitude+0.01
latsouth <- tassites$Latitude-0.01
lonwest <- tassites$Longitude+0.01
loneast <- tassites$Longitude-0.01





#We will give a single lat lon and have the values round to nearest 0.05 for latlow, lathigh, lonlow, lonhigh
yrs <- 1900:2020

precip.files <- vector("list", 121*10)
for (i in 1:length(yrs)) {
  for (j in 1:length(tassites$SiteID)) {
  precip.files[[i]][j] <- paste0("https://dapds00.nci.org.au/thredds/ncss/zv2/agcd/v1/precip/total/r005/01day/agcd_v1_precip_total_r005_daily_",yrs[i],".nc?var=precip&north=",latnorth[j],"&west=",lonwest[j],"&east=",loneast[j],"&south=",latsouth[j],"&horizStride=1&time_start=",yrs[i],"-01-01T09%3A00%3A00Z&time_end=",yrs[i],"-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
  }
}

ppt <- unlist(precip.files)
ppt



?list



tmp <- nc_open("/Users/justinmathias/Desktop/untitled folder/agcd_v1_precip_total_r005_daily_1900.nc?var=precip&north=-41.14242&west=148.04657&east=148.09657&south=-41.19242&horizStride=1&time_start=1900-01-01T09%3A00%3A00Z&time_end=1900-12-31T09%3A00%3A00Z&timeStride=100&addLatLon=true")
tmp
ncvar_get(tmp, "precip")
