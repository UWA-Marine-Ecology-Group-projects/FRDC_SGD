
# libraries ----
library(ncdf4)
library(raster)
library(chron)
library(data.table) # for as.data.frame.table function
library(plyr) # for adply function to turn array into df
library(readr) # for Rds files
library(gapfill) # to convert arrays to raster
library(lubridate) # to convert dates
library(ncdump)
library(dplyr)
library(tidyverse)

rm(list=ls())

# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
dr.dir <- paste(w.dir, "data/raw", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep='/')
s.dir <- paste(w.dir, "data/spatial", sep ='/')



md <- ncdump::NetCDF(paste(dr.dir, 'SST_tworocks_multi.nc', sep='/'))
md

ncfile <- nc_open(paste(dr.dir, 'SST_tworocks_multi.nc', sep='/'))

lat <- ncvar_get(ncfile, "lat")
lon <- ncvar_get(ncfile, "lon")
time <- ncvar_get(ncfile, "time")
date <- as.data.frame(as.POSIXlt(time, origin = '1981-01-01 00:00:00'))
date

ncfile$var$sea_surface_temperature

sst <- ncvar_get(ncfile, "sea_surface_temperature")
sst
v1 <- ncfile$var[[1]]
v1 # also SST
class(v1)
varsize <- v1$varsize
varsize
id <- v1$id

nc_close(ncfile)


# transform to data frame ----

## sea surface temp
sst.df = NULL

for (i in 1:length(time)) {
  
  ssta = data.frame((lon), sst[,,i] %>% as.data.frame())%>% 
    mutate_at(c('X.lon.'), as.factor) %>%
    #as_tibble() %>% 
    pivot_longer(cols = starts_with("V"), names_to = "lat.id", values_to ="temp") %>%
    #gather(key = "key", value = "lon", 1:17) %>% 
    #pivot_longer(ssta, values_to = "sst", 1:17) %>% 
    mutate(time = date[i,], latitude = rep(as.character(lat), 5)) #%>% select(time, lon, lat, sst)
  
  #sst.df = sst.df %>% bind_rows(ssta)
  sst.df = sst.df %>% rbind(ssta)
  #sst.df =  bind_rows(ssta)
}

sst.df <- as.data.frame(sst.df)
head(sst.df)
str(sst.df)

sst.df2 <- sst.df 

### UP TO HERE ----

sst.df2 <- sst.df2 %>%
  mutate(temp.celcius = temp-273.15) %>% # rename column with temp
  mutate(month = month(time)) %>%
  mutate(year = year(time)) %>%
  mutate_at(c('month', 'year', 'latitude'), as.factor) %>%
  dplyr::mutate(month = fct_recode(month, 'Jan'='1', 'Feb'='2', 'Mar'='3', 'Apr'='4', 'May'='5', 'Jun'='6', 'Jul'='7', 'Aug'='8', 
                                   'Sep'='9', 'Oct'='10', 'Nov'='11', 'Dec'='12'))


names(sst.df) <- c("temp" , "key" , "lon" , "time" , "lat")

head(sst.df2)
str(sst.df2)


## try getting the raster file ----

# read potential natural vegetation data sage_veg30.nc:
# modify the following path to reflect local files
vegtype_path <- dr.dir
vegtype_name <- "SST_tworocks_multi.nc"
vegtype_file <- paste(vegtype_path, vegtype_name, sep="/")
vegtype <- raster(vegtype_file, varname="sea_surface_temperature")
plot(vegtype)

