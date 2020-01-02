library("geosphere")
RNGversion("3.5.1")
set.seed(12345)
#setwd("/home/vikbl327/Courses/TDDE01/tdde01-labs/LAB3/")
setwd("C:\\Users\\Victor\\Documents\\R Projects\\tdde01-labs\\LAB3")


stations <- read.csv("stations.csv", sep = ",")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
n = dim(st)[1]


############################Chosen values###############################
lat <- 58.6462
long <- 15.5748
date = "2019-12-24"
t = as.character(seq(4,24, 2))
times = strptime(paste(t, ":00:00", sep=""), format = "%T")
h_distance <-400000 
h_date <-7
h_time <-3
#########################################################################

kernel_function <- function(deltas, h){
  u = deltas/h
  k = exp(-(u^2))
}
temp <- vector(length=length(times))
m = matrix(c(st$longitude, st$latitude), ncol=2)
distances = distHaversine(c(long, lat), m)
date_diffs = as.numeric(difftime(date, st$date),units = "days") %% 365
kernel.dist = kernel_function(distances, h_distance)
plot(distances, kernel.dist)

kernel.dates = kernel_function(date_diffs, h_date)
plot(date_diffs, kernel.dates)

timestamps = as.POSIXct(st$time, format="%H:%M:%S")

for (i in 1:length(times)){
  timediffs = abs(as.numeric(difftime(times[i], timestamps), units = "hours"))
  kernel.time = kernel_function(timediffs, h_time)
  k = kernel.dist+kernel.dates+kernel.time
  temp[i] = sum(k*st$air_temperature)/sum(k)
}

plot(seq(4,24,2), temp, col="blue", 
     main = "Predicted temperatures for 2019-12-31", xlab = "Time of day",
     ylab = "Temperature")
lines(seq(4,24,2), temp, col="blue")
>>>>>>> a6188615d1f17b3259b52f2c45d1c88afbb553da








