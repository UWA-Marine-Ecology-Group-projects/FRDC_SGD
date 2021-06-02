### Plotting bore water temperature time series ----

# clear environment ----
rm(list = ls())


# libraries ----
library( dplyr)
library( rgdal)
library( sp)
library( raster)
library( ggplot2)
library( stringr)
library( lubridate)


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
dr.dir <- paste(w.dir, "data/raw", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep='/')
s.dir <- paste(w.dir, "data/spatial", sep ='/')


# read site metadata ----

df <- read.csv(paste(dr.dir, "Bore_locations_WA.csv", sep ='/'))%>% 
  dplyr::mutate_at(c('Site.Ref', 'Site.name'), as.factor) %>%
  #select(Site.Ref, Site.name, Site.short.name, Latitude, Longitude) %>%
  glimpse()

length(levels(df$Site.Ref))


# read different files with sampling data ----
df1 <- read.csv(paste(dr.dir, "Collection1_WaterQualityContinuousForSiteFlatFile.csv", sep='/')) %>% 
  dplyr::mutate_at(c('Site.Ref', 'Variable.Name', 'Quality'), as.factor) %>%
  glimpse()

df2 <- read.csv(paste(dr.dir, "Collection2_WaterQualityContinuousForSiteFlatFile.csv", sep='/')) %>% 
  dplyr::mutate_at(c('Site.Ref', 'Variable.Name', 'Quality'), as.factor) %>%
  glimpse()

# df3 <- read.csv(paste(dr.dir, "Collection3_LakeThetis_WaterQualityContinuousForSiteFlatFile.csv", sep='/')) %>% 
#   dplyr::mutate_at(c('Site.Ref', 'Variable.Name', 'Quality'), as.factor) %>%
#   glimpse()

# merge the different survey data sheets ----
df3 <- rbind(df1, df2) %>%
 dplyr::inner_join(df) %>% # merge with selected column from df to give name to sites
 #mutate(Collected.Date = as.Date(Collected.Date, format = '%d/%m/%y')) %>%
 mutate(day = str_sub(Collected.Date, -10, -9)) %>% 
 mutate(month = str_sub(Collected.Date, -7, -6)) %>% 
 mutate(year = str_sub(Collected.Date, start= -4)) %>%
 mutate(temp = as.numeric(Value)) %>%
 dplyr::arrange(Collected.Date, desc(Site.Ref)) %>%
 filter(Variable.Name == "Water Temp situ(øC) MAX" | 
          Variable.Name == "Water Temp situ(øC) MEAN" | 
          Variable.Name == "Water Temp situ(øC) MIN") %>%
 glimpse()


# save new df ----
#write.csv(df3, paste(dt.dir, "Borewater_Temp.csv", sep='/'))



### Get averages per month and plot ----

# read data ----
df <- read.csv(paste(dt.dir, "Borewater_Temp.csv", sep='/')) %>%
  dplyr::mutate_at(c('Site.Ref', 'Variable.Name', 'Quality', 'Site.name', 'day', 'month', 'year'), as.factor) %>%
  mutate(date = dmy(Collected.Date)) %>%
  mutate(month2 = recode_factor(month, "1"='01', "2"='02', "3"='03', "4"='04', "5"='05', "6"='06', "7"='07', "8"='08', 
                                "9"='09', "10"='10', "11"='11', "12"='12')) %>%
  mutate(month.year = paste0(month2, '/', year)) %>%
  dplyr::mutate_at('month.year', as.factor) %>%
  glimpse()
  
str(df)


# To Plot ----

# Get averages per site, and month ----


df.sum <- df %>% 
  #mutate(month2 = case_when(month %in% my.vector ~ paste('0', month, sep =''), TRUE ~ paste0(month))) %>% #glimpse()
  #dplyr::mutate_at(c('day', 'month', 'year'), as.character) %>%
  #dplyr::mutate_at(c('day', 'month', 'year'), as.numeric) %>%
  #arrange(month2, year) %>%
  #mutate(month2 = lubridate::month(date)) %>%
  #mutate(month.year = paste0(month2, '/', year)) %>%
  #arrange(desc(month, year)) %>%
  #mutate(num.month.year = month.year) %>%
  #mutate_at(num.month.year, as.character) %>%
  #mutate(num.month.year, as.numeric) %>%
  dplyr::mutate(date2 = lubridate::round_date(date, unit = 'month')) %>% 
  filter(Variable.Name == "Water Temp situ(øC) MEAN") %>%
  group_by(Site.name, date2) %>%
  summarize(av.temp = mean(temp)) %>%
  #mutate(month.year = as.Date(month.year, format = '%m/%y')) %>%
  glimpse()

write.csv(df.sum, paste(dt.dir, "Bore_Temp_summary.csv", sep='/'))


# PLOT ----

ynames <- levels(df.sum$Site.name)

# create directory for plots --
dir.create(paste(p.dir, "bore.temp.monthly.ave", sep='/'))
px.dir <- paste(p.dir, "bore.temp.monthly.ave", sep='/')

for(i in 1:length(levels(df.sum$Site.name))) {
  tiff(paste(px.dir, paste(ynames[i], "tiff", sep='.'), sep='/'), width = 3, height = 3.5, units = 'in', res = 600)
  print(
    ggplot(df.sum %>% filter(Site.name == ynames[i]), aes(x = date2, y = av.temp)) +
      geom_point() +
      #ylim(19, 22) +
    labs(title = ynames[i])+
    theme(axis.text = element_text(size = 6, angle = 90))
  )
  dev.off()
}


## Save site with unique coordinates ----
# read data ----
df <- read.csv(paste(dt.dir, "Borewater_Temp.csv", sep='/')) %>%
  dplyr::mutate_at(c('Site.Ref', 'Variable.Name', 'Quality', 'Site.name', 'day', 'month', 'year'), as.factor) %>%
  mutate(date = dmy(Collected.Date)) %>%
  mutate(month2 = recode_factor(month, "1"='01', "2"='02', "3"='03', "4"='04', "5"='05', "6"='06', "7"='07', "8"='08', 
                                "9"='09', "10"='10', "11"='11', "12"='12')) %>%
  mutate(month.year = paste0(month2, '/', year)) %>%
  dplyr::mutate_at('month.year', as.factor) %>%
  glimpse()

str(df)

summary(df)



df2 <- df %>% 
  group_by(Site.name) %>%
  slice(which.max(temp)) %>%
  glimpse()

str(df2)


write.csv(df2, paste(s.dir, "Bore_sites_w_temp.csv", sep='/'))
