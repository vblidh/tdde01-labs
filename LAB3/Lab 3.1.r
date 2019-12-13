library("geosphere")
RNGversion("3.5.1")
set.seed(12345)
setwd("/home/vikbl327/Courses/TDDE01/tdde01-labs/LAB3/")


stations <- read.csv("stations.csv", sep = ",")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")


############################Chosen values###############################
lat <- 58.4274 
long <- 14.826
date = "2017-12-31"
date.numeric = as.integer(unlist(strsplit(date, "-")))
as.character.Date(st$date)


#########################################################################

m = matrix(c(st$latitude, st$longitude), ncol=2)
distances = distHaversine(c(lat, long), m)






