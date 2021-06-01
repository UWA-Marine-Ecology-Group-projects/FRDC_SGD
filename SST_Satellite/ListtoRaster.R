######
### R script to get the temperature data from Jess 
### 1. Extract max temp for each year
### 2. Makes a raster file
### 3. Saves it
### 4. Converts Kelvin to Centigrade
### 5. Saves that new raster file
### Maximum summer temperature data for 2010 and 2011

require(R.matlab)
library(R.matlab)


library(maptools)
library(raster) 
library(rgdal)
library(sp)

setwd("I:/R_Ch4/FromJess")
#get_variables_ncoutputs.mat --> find in matlab folders to run if don't already have the data
dat <- readMat("I:/R_Ch4/FromJess/max_summer_wa.mat") #put own folder where you downloaded here
class(dat) # dat is a list
str(dat) # check the elements of the list


lat <- dat[[1]]
lon <- dat[[2]]
summer_max <- dat[[3]]
years <- dat[[4]]

head(dat)
str(dat)
head(dat$years.max)  # years from 1993 - 2017

class(lat)
class(years)

## looking at the lists and elements
print(summer_max[1,2,]) # print the element in the 1st row, second column of the 25th matrix (of 2017)
print(lat[800])
lat
lon


# https://stackoverflow.com/questions/14513480/convert-matrix-to-raster-in-r


##### extract the max temps from one year

# make a list with lons lats and the temp data from the year needed
maxt94 <- list(lat, lon, summer_max[,,2])
test <- list(summer_max[lat,lon, 2], na.omit(summer_max))
str(test)
str(maxt94)
# name elements of the list
names(maxt94) <- c("y","x","z")
str(maxt94)
# make raster
#r <- raster(maxt94, crs = CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m"))

##### to make raster file ####
# https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation

x1 <- range(dat$lon)[1]
x2 <- range (dat$lon)[2]
y1 <- range(dat$lat)[1]
y2 <- range (dat$lat)[2]

class(y2)

r

r3 <- raster(
  maxt94$z,
  y1, y2,
  x1, x2,
  crs = CRS("+proj=cea  +datum=WGS84 +units=m")
)

plot(r3)
r3
r3x <- flip(r3, direction = "x")
plot(r3x)
r
tr <- t(r3)
plot(tr)


ncol(r)
t(r)
plot(r)



###### Extract, plot and save 2011 data ####

years # check what number is 2011

##### make array of 2011 ####
maxt2011 <- list(lat, lon, summer_max[,,19])
class(maxt2011)
str(maxt2011)

# name the elements
names(maxt2011) <- c("y","x","z")
str(maxt2011)

##### Make 2011 raster ####

# give limits of the lats and longs for raster creation
x1 <- range(dat$lat)[1]
x2 <- range (dat$lat)[2]
y1 <- range(dat$lon)[1]
y2 <- range (dat$lon)[2]


r11 <- raster(
  maxt2011$z,
  x1, x2,
  y1, y2,
  crs = CRS("+proj=cea +datum=WGS84 +units=m")
)

plot(r11)
proj4string(r11)
class(r11)
r11

# transpose values
rt11 <-t(r11)
plot(rt11)
class(rt11)
rt11

##### Save 2011 data raster ####
writeRaster(rt11, "maxt2011.tif", format = "GTiff") # this works
class(rt11)
# check
test2011 <- raster("maxt2011.tif")
plot(test2011)

############################################
###### Extract, plot and save 2010 data ####

#### make array of 2010 ####
maxt2010 <- list(lat, lon, summer_max[,,18])
class(maxt2010)
str(maxt2010)

# name the elements
names(maxt2010) <- c("y","x","z")
str(maxt2010)

##### Create 2010 raster ####
# give limits of the lats and longs for raster creation
x1 <- range(dat$lat)[1]
x2 <- range (dat$lat)[2]
y1 <- range(dat$lon)[1]
y2 <- range (dat$lon)[2]


r10 <- raster(
  maxt2010$z,
  x1, x2,
  y1, y2,
  crs = CRS("+proj=cea +datum=WGS84 +units=m")
)

plot(r10)

# transpose values
rt10 <-t(r10)
plot(rt10)

##### Save 2010 plot ####

class(rt10)
rt10
writeRaster(rt10, "maxt2010.tif", format = "GTiff") # this works
# check it worked
test2010 <- raster("maxt2010.tif")
plot(test2010)

##################################################
##### Change values from Kelvin to Centigrade ####

# to convert Kelvin to Centigrades: substract 273.15 from kelvin
convertfact <- 273.15

# create function
ktoc <- function(x){ x - convertfact}

# apply to raster 

#### 2010 ####
r10C <- calc(test2010, ktoc)
plot(r10C)
# save new raster file
writeRaster(r10C, "maxt2010C.tif", format = "GTiff") # this works

#### 2011 ####
r11C <- calc(test2011, ktoc)
plot(r11C)
# save new raster file
writeRaster(r11C, "maxt2011C.tif", format = "GTiff") # this works
