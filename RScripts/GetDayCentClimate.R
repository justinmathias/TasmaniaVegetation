#Created by Justin Mathias, 5/1/2023
#This code will create a weather file for a single site using thredds data portal using John Abatzaglou's data

library('easypackages')
libraries(c('tidyverse', 'ncdf4', 'lubridate', 'rlist','data.table','stringi')) #'HelpersMG'

# #Define sites
# tassites <- data.frame('SiteID' = c('TheKeepDry', 'TheKeepWet','HolwellDry','HolwellWet', 'MinnowCreek', 'Meander', 'MoleCreek','Hellyer','Smithton','Poilinna'),
#                        'Lat' = c(-41.16737, -41.17169, -41.25499, -41.27372, -41.43795, -41.70001, -41.57743, -41.27375, -41.04367, -40.97341),
#                        'Lon' = c(148.07137, 148.06780, 146.77529, 146.76602, 146.42546, 146.58134, 146.25085, 145.61582, 145.12642, 145.01781))
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
precip_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/precip/")
tmax_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmax/")
tmin_fls <- list.files("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmin/")

#Create weather files----
#Preallocate list
precip_out <- list()
tmax_out <- list()
tmin_out <- list()

{
  #Loop through precip----
  for (i in seq_along(precip_fls)) {
    
    #Open ncdf file and get variable, lat, lon, time
    tmp <- nc_open(paste0("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/precip/",precip_fls[i]))
    
    lat <- ncvar_get(tmp, "lat")
    lon <- ncvar_get(tmp, "lon")
    time <- ncvar_get(tmp, 'time')
    precip <- ncvar_get(tmp, "precip")
    
    #Create empty data frame for storing variables
    dat <- data.frame(matrix(nrow = length(time), ncol = 6)) %>% rename("day" = "X1", "month" = "X2", "year" = "X3", "jday" = "X4", "precip" = "X5", "file" = "X6")
    #Create empty list to store data in
    
    
    #Extract day, month, year, and julian day as integers using lubridate.
    #This assumes all files are in standard format of days since 1850-01-01
    for (j in 1:length(time)) {
      dat$day[j] <- day(as_date(-43829 +time[j])) #Assign day of month
      dat$month[j] <- month(as_date(-43829 +time[j])) #Assign month of year
      dat$year[j] <- year(as_date(-43829 +time[j])) #Assign year
      dat$jday[j] <- yday(as_date(-43829 +time[j])) #Assign julian day
      dat$file[j] <- precip_fls[i] #Record which file these data are from
      
      dat$precip[j] <- ifelse(length(lat) == 2 & length(lon) == 2, mean(precip[,,j]), #This will deal with cases where there are only 1 lat/lon
                              ifelse(length(lat) == 1 & length(lon) == 2, mean(precip[,j]), 
                                     ifelse(length(lat) == 2 & length(lon) == 1, mean(precip[,j]), "NA")))
    }
    precip_out[[i]] <-  dat #Append climate for each data frame to list. N in list should equal N files.
  }
  
  #Turn precip list into data frame
  precip_final <- list.rbind(precip_out)
  
  setwd("/Users/justinmathias/Desktop")
  write.csv(precip_final, "precip_final.csv")
}



{
  #Loop through tmax----
  for (i in seq_along(tmax_fls)) {
    
    #Open ncdf file and get variable, lat, lon, time
    tmp <- nc_open(paste0("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmax/",tmax_fls[i]))
    
    lat <- ncvar_get(tmp, "lat")
    lon <- ncvar_get(tmp, "lon")
    time <- ncvar_get(tmp, 'time')
    tmax <- ncvar_get(tmp, "tmax")
    
    #Create empty data frame for storing variables
    dat <- data.frame(matrix(nrow = length(time), ncol = 6)) %>% rename("day" = "X1", "month" = "X2", "year" = "X3", "jday" = "X4", "tmax" = "X5", "file" = "X6")
    #Create empty list to store data in
    
    
    #Extract day, month, year, and julian day as integers using lubridate.
    #This assumes all files are in standard format of days since 1850-01-01
    for (j in 1:length(time)) {
      dat$day[j] <- day(as_date(-43829 +time[j])) #Assign day of month
      dat$month[j] <- month(as_date(-43829 +time[j])) #Assign month of year
      dat$year[j] <- year(as_date(-43829 +time[j])) #Assign year
      dat$jday[j] <- yday(as_date(-43829 +time[j])) #Assign julian day
      dat$file[j] <- tmax_fls[i] #Record which file these data are from
      
      dat$tmax[j] <- ifelse(length(lat) == 2 & length(lon) == 2, mean(tmax[,,j]), #This will deal with cases where there are only 1 lat/lon
                            ifelse(length(lat) == 1 & length(lon) == 2, mean(tmax[,j]), 
                                   ifelse(length(lat) == 2 & length(lon) == 1, mean(tmax[,j]), "NA")))
    }
    tmax_out[[i]] <-  dat #Append climate for each data frame to list. N in list should equal N files.
  }
  
  #Turn tmax list into data frame
  tmax_final <- list.rbind(tmax_out)
  
  setwd("/Users/justinmathias/Desktop")
  write.csv(tmax_final, "tmax_final.csv")
}





{
  #Loop through tmin----
  for (i in seq_along(tmin_fls)) {
    
    #Open ncdf file and get variable, lat, lon, time
    tmp <- nc_open(paste0("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/NC Files/tmin/",tmin_fls[i]))
    
    lat <- ncvar_get(tmp, "lat")
    lon <- ncvar_get(tmp, "lon")
    time <- ncvar_get(tmp, 'time')
    tmin <- ncvar_get(tmp, "tmin")
    
    #Create empty data frame for storing variables
    dat <- data.frame(matrix(nrow = length(time), ncol = 6)) %>% rename("day" = "X1", "month" = "X2", "year" = "X3", "jday" = "X4", "tmin" = "X5", "file" = "X6")
    #Create empty list to store data in
    
    
    #Extract day, month, year, and julian day as integers using lubridate.
    #This assumes all files are in standard format of days since 1850-01-01
    for (j in 1:length(time)) {
      dat$day[j] <- day(as_date(-43829 +time[j])) #Assign day of month
      dat$month[j] <- month(as_date(-43829 +time[j])) #Assign month of year
      dat$year[j] <- year(as_date(-43829 +time[j])) #Assign year
      dat$jday[j] <- yday(as_date(-43829 +time[j])) #Assign julian day
      dat$file[j] <- tmin_fls[i] #Record which file these data are from
      
      dat$tmin[j] <- ifelse(length(lat) == 2 & length(lon) == 2, mean(tmin[,,j]), #This will deal with cases where there are only 1 lat/lon
                            ifelse(length(lat) == 1 & length(lon) == 2, mean(tmin[,j]), 
                                   ifelse(length(lat) == 2 & length(lon) == 1, mean(tmin[,j]), "NA")))
    }
    tmin_out[[i]] <-  dat #Append climate for each data frame to list. N in list should equal N files.
  }
  
  #Turn tmin list into data frame
  tmin_final <- list.rbind(tmin_out)
  
  setwd("/Users/justinmathias/Desktop")
  write.csv(tmin_final, "tmin_final.csv")
}

#Read in csv files----
precip_final <- fread("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/output files/precip_final.csv")
tmax_final <- fread("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/output files/tmax_final.csv")
tmin_final <- fread("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/output files/tmin_final.csv")


#Wrangle data to merge files----
#Here, we are going to have to play games to get file names to match. we are going to rename tmax and tmin file names to precip for merging.
#Data will remain unaltered, but this is the quickest solution.
tmax_final$file <- stri_replace_all_regex(tmax_final$file, #replace tmax and mean with precip and total, respectively
                       pattern=c('tmax','mean'),
                       replacement=c('precip', 'total'),
                       vectorize=FALSE)

tmin_final$file <- stri_replace_all_regex(tmin_final$file, #replace tmin and mean with precip and total, respectively
                                          pattern=c('tmin','mean'),
                                          replacement=c('precip', 'total'),
                                          vectorize=FALSE)


#Now merge!----
weather <- list(precip_final, tmax_final, tmin_final) %>% reduce(left_join)

#Put in clean format for DayCent before splitting for each site. 
#Still need to include file name at this point. Round to two decimal points
wth <- weather %>% 
  mutate(tmax = round(tmax, 2),
         tmin = round(tmin, 2),
         precip = round(precip, 2),
         site = paste0("north=",sub(".*&north=(.{55}).*", "\\1", file))) %>% 
  select(day,month,year,jday,tmax,tmin,precip,site)

#Create lookup table to rename latlon to site
lookup.table <- c("north=-40.96341&west=145.00781&east=145.02781&south=-40.98341" = "Poilinna",
                  "north=-41.03367&west=145.11642&east=145.13642&south=-41.05367" = "Smithton",
                  "north=-41.15737&west=148.06137&east=148.08137&south=-41.17737" = "TheKeepDry",
                  "north=-41.16169&west=148.0578&east=148.0778&south=-41.18169&d" = "TheKeepWet",
                  "north=-41.24499&west=146.76529&east=146.78529&south=-41.26499" = "HolwellDry",
                  "north=-41.26372&west=146.75602&east=146.77602&south=-41.28372" = "HolwellWet",
                  "north=-41.26375&west=145.60582&east=145.62582&south=-41.28375" = "Hellyer",
                  "north=-41.42795&west=146.41546&east=146.43546&south=-41.44795" = "MinnowCreek",
                  "north=-41.56743&west=146.24085&east=146.26085&south=-41.58743" = "MoleCreek",
                  "north=-41.69001&west=146.57134&east=146.59134&south=-41.71001" = "Meander")

wth$sitename <- lookup.table[wth$site] #Create new column for renamed biomes, referencing original biomes

wth_final <- wth %>% select(-site)
# write.csv(wth_final, "climate_final.csv", row.names = FALSE)
wth_groups <- wth_final %>% group_split(sitename)

#Create weather files!----
assign(paste0(wth_groups[[1]]$sitename[1]) ,wth_groups[[1]] %>% select(-sitename))
assign(paste0(wth_groups[[2]]$sitename[1]) ,wth_groups[[2]] %>% select(-sitename))
assign(paste0(wth_groups[[3]]$sitename[1]) ,wth_groups[[3]] %>% select(-sitename))
assign(paste0(wth_groups[[4]]$sitename[1]) ,wth_groups[[4]] %>% select(-sitename))
assign(paste0(wth_groups[[5]]$sitename[1]) ,wth_groups[[5]] %>% select(-sitename))
assign(paste0(wth_groups[[6]]$sitename[1]) ,wth_groups[[6]] %>% select(-sitename))
assign(paste0(wth_groups[[7]]$sitename[1]) ,wth_groups[[7]] %>% select(-sitename))
assign(paste0(wth_groups[[8]]$sitename[1]) ,wth_groups[[8]] %>% select(-sitename))
assign(paste0(wth_groups[[9]]$sitename[1]) ,wth_groups[[9]] %>% select(-sitename))
assign(paste0(wth_groups[[10]]$sitename[1]) ,wth_groups[[10]] %>% select(-sitename))

setwd("/Users/justinmathias/Library/CloudStorage/Dropbox/Research/UIdaho Postdoc/Tasmania/TasmaniaVegetation/Data/ClimateData/output files")

write.table(Hellyer, "Hellyer.wth", col.names=F,row.names=F,quote=F)
write.table(HolwellDry, "HolwellDry.wth", col.names=F,row.names=F,quote=F)
write.table(HolwellWet, "HolwellWet.wth", col.names=F,row.names=F,quote=F)
write.table(Meander, "Meander.wth", col.names=F,row.names=F,quote=F)
write.table(MinnowCreek, "MinnowCreek.wth", col.names=F,row.names=F,quote=F)
write.table(MoleCreek, "MoleCreek.wth", col.names=F,row.names=F,quote=F)
write.table(Poilinna, "Poilinna.wth", col.names=F,row.names=F,quote=F)
write.table(Smithton, "Smithton.wth", col.names=F,row.names=F,quote=F)
write.table(TheKeepDry, "TheKeepDry.wth", col.names=F,row.names=F,quote=F)
write.table(TheKeepWet, "TheKeepWet.wth", col.names=F,row.names=F,quote=F)


