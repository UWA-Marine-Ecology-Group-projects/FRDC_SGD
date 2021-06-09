#### This script is to extract NetCDF files

# Sources:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

# clear r memory
rm(list=ls())

# open library to work with cdf
library(ncdf4)
library(raster)
library(chron)
library(data.table) # for as.data.frame.table function
library(plyr) # for adply function to turn array into df
library(readr) # for Rds files
library(gapfill) # to convert arrays to raster
library(lubridate) # to convert dates


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
dr.dir <- paste(w.dir, "data/raw", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep='/')
s.dir <- paste(w.dir, "data/spatial", sep ='/')



file <- nc_open(paste(dr.dir, 'SST_cervantes_multi.nc', sep='/'))
class(file)

attributes(file$var)
SST <- file$var[[1]]
class(SST)
file$var$sea_surface_temperature


v1 <- file$var[[1]]
v1 # also SST
class(v1)
v1[1]$id

varsize <- v1$varsize
varsize # 2  1 12
class(varsize)
ndims   <- v1$ndims
ndims # 3
class(ndims)
nt      <- varsize[ndims]
nt # 12


lats <- ncvar_get(file,'lat')
head(lats)
dim(lats) # 285
class(lats) # array
# make data frame
library(data.table) # this works well
lat <- as.data.frame.table(lats)
head(lat)
str(lat)
# save
write.csv2(lat, file = "SST_lats.csv")


lons <- ncvar_get(file,'lon')
head(lons) # 725
class(lons) # array
# make data frame
lon <- as.data.frame.table(lons)
head(lon)
str(lon)
# save the data frame to later replace values in SST data frame
write.csv2(lon, file = "SST_lons.csv")


time <- ncvar_get(file,'time')
time # check the time format
head(time)
dim(time) #9763
class(time) # array
tunits <- ncatt_get(file,"time","units")
tunits # check units : seconds since 1981-01-01 00:00:00
class(tunits) # "list"
nt <- dim(time)
nt # 9763
# change to actual date --
t <- as.data.frame(as.POSIXlt(time, origin = '1981-01-01 00:00:00'))
class(t)
# make data frame
#t <- as.data.frame.table(time2)
head(t)
str(t)
# save
write.csv2(t, file = "SST_times.csv") # as seconds since "1981-01-01" "00:00:00"

# get global attributes of net cdf
title <- ncatt_get(file, 0, "title")
institution <- ncatt_get(file, 0, "institution")
datasource <- ncatt_get(file, 0, "source")
references <- ncatt_get(file, 0, "references")
history <- ncatt_get(file, 0, "history")
Conventions <- ncatt_get(file, 0, "Conventions")

### CLOSE NETCFD FILE!
nc_close(file)


#### at this point I start extracting the SST as time fragments and saving as dataframe to later join them
file <- nc_open(paste(dr.dir, 'IMOS_aggregation_20210601T022602Z.nc', sep='/')) # open netcdf file
SST <- file$var[[1]] # object the sst variable

# choose the time frame you want to download
start <- c(1,1,1)
count <- c(2,1,12) ## gonna start with only 5 time steps
test <- ncvar_get(file, SST)
test

## check these the first time
dlname <- ncatt_get(file, v1, "long_name") # "sea surface skin temperature"
dunits <- ncatt_get(file, v1, "units")  # "kelvin"
fillvalue <- ncatt_get(file, v1, "_FillValue") #  9.96921e+36
class(test) #array

### close the netcdf file!
nc_close(file)

# Replace NetCDF fillvalues with R NAs in the ARRAY
test[test == fillvalue$value] <- NA
length(na.omit(as.vector(test[, , 1]))) # check how many NAs

## transform array into data frame
dftest1 <- as.data.frame.table(test)


#### Test this function ### Ask Bin how to make this better
# clear r memory
rm(list=ls())

## get sst in chunks of 500 time steps, 1000 is too large
start1 <- c(1,1,1)
name1 <- paste("start1")
start2 <- c(1,1,501)
name2 <- paste("start2")
start3 <- c(1,1,1002)
name3 <- paste("start3")
start4 <- c(1,1,1503)
name4 <- paste("start4")
start5 <- c(1,1,2004)
name5 <- paste("start5")
start6 <- c(1,1,2505)
name6 <- paste("start6")
start7 <- c(1,1,3006)
name7 <- paste("start7")
start8 <- c(1,1,3507)
name8 <- paste("start8")
start9 <- c(1,1,4008)
name9 <- paste("start9")
start10 <- c(1,1,4509)
name10 <- paste("start10")
start11 <- c(1,1,5010)
name11 <- paste("start11")
start12 <- c(1,1,5511)
name12 <- paste("start12")
start13 <- c(1,1,6012)
name13 <- paste("start13")
start14 <- c(1,1,6513)
name14 <- paste("start14")
start15 <- c(1,1,7014)
name15 <- paste("start15")
start16 <- c(1,1,7515)
name16 <- paste("start16")
start17 <- c(1,1,8016)
name17 <- paste("start17")
start18 <- c(1,1,8517)
name18 <- paste("start18")
start19 <- c(1,1,9018)
name19 <- paste("start19")
start20 <- c(1,1,9519)
name20 <- paste("start20")

# tests
#starttest <- c(1,1,2000)
#nametest <- paste("starttest")

#### the following function saves the files as csv, rds, or raster

SSTprep <- function(start, name){
  # Open the NetCDF connection
  file <- nc_open('IMOS_aggregation_20190503T032754Z.nc') # open netcdf file
  # SST object variable
  SST <- file$var[[1]]
  # define count
  count <- c(285,725,500)
  #extractSST
  testf <- ncvar_get(file,SST, start, count)  
  # round values of sst to 3 decimal places
  testf <- round(testf, 3)
  # transfor into data frame using as.data.frame.table
  dftestf1 <- as.data.frame.table(testf)
  # rearrange long and lat
  input_array <- aperm(testf, c(2,1,3))
  # transfor into data frame using adply
  #create Raster stack: need to provide n for lats, longs and time
  output_stack <- stack(brick(array(input_array, c(725,285,5))))
  #dftestf1 <- adply(testf, c(1,2,3))
  # Close the NetCDF connection and finish
  nc_close(file)
  #return(dftestf1)
  # Save as Raster stack
  writeRaster(output_stack, filename = paste0(name,".tif")) # layer names rely on stack name 
  #save as csv
  #write.csv(dftestf1, file = paste0(name, ".csv")) # as seconds since "1981-01-01" "00:00:00"
  # save as RDS
  #saveRDS(dftestf1, file = paste0("I:/SST_IMOS/SST_data/",name,".Rda")) # .Rds files are smaller than .csv
  # clear r memory
  rm(list=ls())
 
}

#test
#SSTprep(starttest, nametest)


SSTprep(start1, name1)
SSTprep(start2, name2)
SSTprep(start3, name3)
SSTprep(start4, name4) ## this works, but when loaded in R is still huge ~2 gb each
SSTprep(start5, name5)
SSTprep(start6, name6)
SSTprep(start7, name7)
SSTprep(start8, name8)
SSTprep(start9, name9)
### also, do I need to match lats, longs and times before saving as Rda?


dfstart1 <- read_rds("I:/SST_IMOS/SST_data/start1.Rda")
dfstart4 <- read_rds("I:/SST_IMOS/SST_data/start4.Rda")
class(sss)
str(sss)
head(sss)
#test
dfstarttest <- read_rds("I:/SST_IMOS/SST_data/starttest.Rda")


##### package ff seems to be good for managing large files
# https://rpubs.com/msundar/large_data_analysis
library(ff)
# http://r.789695.n4.nabble.com/Reading-in-csv-data-with-ff-package-td4680708.html
# need to define the colClasses because characters are not accepted in ff
# SO, PERHAPS BETTER TO TRANSFORM DATA FRAME ISING THE APLYR FUNCTION?
sss1 <- read.csv.ffdf(file = "I:/SST_IMOS/SST_data/starttest.csv", colClasses = c("factor", "factor", "factor","factor","numeric"))
class(sss1)
head(sss1)
str(sss1)
sss2 <- read_rds("I:/SST_IMOS/SST_data/starttest.Rda")

###
### TEST OF FF PACKAGE USING A DF CREATED WITH THE ADPLY FUNCTION
# resulted in having to classify the same way as.data.frame.table, so gonna keep using the other one
sss1 <- read.csv.ffdf(file = "I:/SST_IMOS/SST_data/starttest.csv", colClasses = c("factor", "factor", "factor","factor","numeric"))
class(sss1)
head(sss1)
str(sss1)
### rbinding the ffdf object
# https://stackoverflow.com/questions/11933287/rbinding-two-ffdf-data-frames-in-r

## TO DO TOMORROW 9/5/19: talk to Jess if I can
# 1. test the ff funcition using the aplyr function and opening it with ff, just to see if it looks the same, or not for R bind
# before proceeding I am going to chat to Jess and Malcolm about this
# 2. decide on how to proceed and save the rest of the sst as csv
# 3. use ff to rbind
# 4. save df as RDA file, 
# 5. try using it with HW package


############
############
#### testing saving the SST as raster file : array to raster stack
## use gapfill package to predict missing values
# clear r memory
rm(list=ls())

# Open the NetCDF connection
file <- nc_open('IMOS_aggregation_20190503T032754Z.nc') # open netcdf file
# SST object variable
SST <- file$var[[1]]
# define count
count <- c(285,725,5)
start <- c(1,1,2000)
#nametest <- paste("starttest")
#extractSST
testf <- ncvar_get(file,SST, start, count)
class(testf)
str(testf)

# round values of sst to 3 decimal places
#testf <- round(testf, 3)
# transform array into raster stack # aperm: Transpose an array by permuting its dimensions and optionally resizing it.
# this is working, but resulting maps for look a bit funny: lats and longs are not as such 
# but I think at this point this isn't really necessary..
# rearrange long and lat
input_array <- aperm(testf, c(2,1,3)) # input_array is still an array 2,1,3 so the map looks normal
str(input_array) # looks the same as testf but lat first than long
# this next will return a raster image (per raster layer, so dont use in large stacks)
Image(input_array, col=rev(terrain.colors(100)), asRaster=T) # Creates an image panel to visualize data in 4, 3 or 2 dime arrays. The function returns a ggplot2 object, which can be modified using ggplot2 

#output <- Gapfill(data=input_array) # seems like this only works in 4d data?

#create Raster stack: need to provide n for lats, longs and time
output_stack <- stack(brick(array(input_array, c(725,285,5))))
plot(output_stack)
names(output_stack)

# now save the raster stack to disc
writeRaster(output_stack, filename = "testrasterstack.tif") # This names the layers as the file of the raster stack plus 1,2,3..
myrastest <- stack("testrasterstack.tif")
names(myrastest) #name of stack.1, 2, 3... 

###### THIS PREVIOUS SESSION WORKS, NOW ADD TO FUNCTION AND TEST




###  testing transforming arrays into data frames
# transfor into data frame using as.data.frame.table
dftestf1 <- as.data.frame.table(testf)
# transfor into data frame using adply
#dftestf1 <- adply(testf, c(1,2,3))
# Close the NetCDF connection and finish
nc_close(file)
#return(dftestf1)
#save as csv
write.csv(dftestf1, file = paste0(name, ".csv")) # as seconds since "1981-01-01" "00:00:00"
saveRDS(dftestf1, file = paste0("I:/SST_IMOS/SST_data/",name,".Rda")) # .Rds files are smaller than .csv
# clear r memory
rm(list=ls())






















######################
### THIS PART IS WHAT I DID BEFORE AND WAS'T WORKING

# save as csv
write.csv2(dftest1, "dftest1.csv") 


### Convert the time variable # this works only in the main net cdf file, not in the array
# The time variable, in ???time-since??? units can be converted into ???real??? (or more easily readable) time values by splitting the 
# time tunits$value string into its component parts, and then using the chron() function to determine the absolute value of each 
# time value from the time origin
head(tunits)
class(tunits)
# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(time, origin = c(tmonth, tday, tyear))
tunits




res <- as.data.frame(reshape::melt.array(test, 'sea_surface_temperature'))
test[ndims]
class(test) # array
str(test) # num [1:285, 1:725, 1:5] NA NA NA
nrow(test) #285
ncol(test)# 725 
colnames(test)
rownames(test)
length(test) # 1033125
dimens <- dim(test)  
dimens
namesdim <- dimnames(test) 
namesdim # NULL - no names for dimensions?
test[1,,]
test[,1,]

# use adply function to transform array into data frame? ## THIS DID NOT WORK, DID NOT GIVE THE CORRECT VALUES
library(plyr)
dftest <- adply(test, c(1,2,3))
class(dftest)
str(dftest)
dftest$X2[[20600]]
summary(dftest)


library(easyNCDF)
ArrayToNc(test, 'test.nc') #saved to original working directory
variables <- test$var[[1]]

nc_create('2test.nc', totest)


##### LAST NIGHT FINISHED HERE

test2 <- nc_open('test.nc')
print(test2)
attributes(test2$var)
test2$ndims
dimens <- test2$dim
dimens
class(test2)
v1 <- test2$var[[1]]
data <- ncvar_get(test2, v1)
lon <- ncvar_get(test2, varid = "dim2")
nlon <- dim(lon)
head(lon)
t <- ncvar_get(test2, "dim3")




v1 <- test2$var[[1]]
v1
varsize <- v1$varsize
varsize
ndims   <- v1$ndims
ndims

test3 <- ncvar_get(test2)
class(test3)

## now we want to save it as data.frame
#dimnames(test3) <- list(lon = test3$dim[[2]],
                 #lat = test3$dim[[1]],
                 #time = test3$dim[[3]]) # this in not working


## I thik I need to change the time before going ahead with this part
test4 <- as.data.frame(reshape2::melt(test3, value.name = "temp"), row.names = NULL) %>% 
  mutate(time = as.Date(as.POSIXct(time, origin = "1970-01-01 00:00:00")),
         temp = round(temp, 2))





#####
## This illustrates how to read data one timestep at a time.
for( i in 1:nt ) {
  # Initialize start and count to read one timestep of the variable.
  start <- rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] <- i	# change to start=(1,1,1,...,i) to read timestep i
  count <- varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] <- 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  data3 <- ncvar_get(file, v1, start=start, count=count )
  
  # Now read in the value of the timelike dimension
  timeval <- ncvar_get(file, v1$dim[[ndims]]$name, start=i, count=1 )
  
  print(paste("Data for variable",v1$name,"at timestep",i,
              " (time value=",timeval,v1$dim[[ndims]]$units,"):"))
  print(data3)
}

nc_close(nc)
######
class(data3)

start <- rep(1,ndims)
start[ndims] <- i
start <- c(1,1,2792) # first lon,first lat,time step no 400
count <- varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
count[ndims] <- 1
count <- c(285,725,1395) #end lon,lat accoring to dimensions of file, time (number of time steps), length of lon vector = 285, lat is 725

sst1 <- ncvar_get(file,'sea_surface_temperature', start, count)
class(sst1)
str(sst1)
str(sst2)
dimnames(test) <- list( lon = file$dim$longitude$vals,
                        lat = file$dim$latitude$vals,
                        time = file$dim$time$vals)
sst2 <- ncvar_get(file,'sea_surface_temperature', start, count)
sst3 <- ncvar_get(file,'sea_surface_temperature', start, count)
dim(sst1)
tmp.vec <- as.vector(sst1)
length(tmp.vec)
str(tmp.vec)
class(tmp.vec)

tmp.mat <- matrix(tmp.vec, nrow = nlon * nlat, ncol = nt)
dim(tmp.mat)
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# Now read in the value of the timelike dimension
timeval <- ncvar_get(file, v3$dim[[ndims]]$name, start=i, count=1 )
class(sst1)
class(sst2)
str(sst2)
lats <- ncvar_get(sst1,'lat')
lons <- ncvar_get(sst1,'lon')
nc_close(file) # alway close a netcdf when you are done with it!

# Get the variable and its attributes, and verify the size of the array
tmp.array <- ncvar_get(data3, dname)
dlname <- ncatt_get(sst1, dname, "long_name")
dunits <- ncatt_get(sst1, dname, "units")
fillvalue <- ncatt_get(sst1, dname, "_FillValue")
dim(sst1)

# convert the time variable
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear)) #this is from library chron
tunits #check units

###########

dname <- "sea_surface_temperature"

# Get the longtiudes and latitudes , using now the ncvar_get() function in ncdf4.
lon <- ncvar_get(sstn2, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(sstn2, "lat", verbose = F)
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

#Get the time variable and its attributes using the ncvar_get() and ncatt_get() functions, 
#and also get the number of times using the dim() function.
t <- ncvar_get(sstn2, "time")
tunits <- ncatt_get(sstn2, "time", "units")
nt <- dim(t)

# convert the time variable
# The time variable, in ???time-since??? units can be converted into ???real??? (or more easily readable) time values
# by splitting the time tunits$value string into its component parts, and then using the chron() function to 
# determine the absolute value of each time value from the time origin.
# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear)) #this is from library chron

##
m <- 2
tmp.slice <- sstn2[, , m]
sstn2


# Replace Netcdf fillvalues with R NAs
# In NetCDF file, values of a variable that are either missing or simply not available 
# (i.e. ocean grid points in a terrestrial data set) are flagged using specific ???fill values??? (_FillValue) or 
# missing values (missing_value), the values of which are set as attributes of a variable. In R, such unavailable 
# data are indicated using the ???NA??? value. The following code fragment illustrates how to replace the NetCDF 
# variable's fill values with R NA's .

tmp.array[tmp.array == fillvalue$value] <- NA ## can't do this if cannot load the data

#Get the variable and its attributes, and verify the size of the array
tmp.array <- ncvar_get(sstn2, dname) ### Error: cannot allocate vector of size 15.0 Gb 
dlname <- ncatt_get(sstn2, dname, "long_name")
dunits <- ncatt_get(sstn2, dname, "units")
fillvalue <- ncatt_get(sstn2, dname, "_FillValue")
dim(tmp.array)




print(sstn2)
attributes(sstn2$var) # this is to see the variables in the netcdf file

sst <- ncvar_get(sstn2, "sea_surface_temperature")

# get sst - this function is from the marine hw tutorial
SST_prep <- function(nc_file){
  
  # Open the NetCDF connection
  nc <- nc_open(nc_file)
  
  # Extract the SST values and add the lon/lat/time dimension names
  res <- ncvar_get(nc, varid = "sea_surface_temperature")
  dimnames(res) <- list(lon = nc$dim$lon$vals,
                        lat = nc$dim$lat$vals,
                        time = nc$dim$time$vals)
  
  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = "1981-01-01 00:00:00")),
           temp = round(temp, 2))
  
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

# Prep the data
SST1_prep <- SST_prep(sstn)
