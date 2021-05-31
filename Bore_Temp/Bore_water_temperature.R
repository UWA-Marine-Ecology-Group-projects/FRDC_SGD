### Plotting bore water temperature time series ----

# clear environment ----
rm(list = ls())


# libraries ----
library( dplyr)
library( rgdal)
library( sp)
library( raster)
library( ggplot2)


# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
dr.dir <- paste(w.dir, "data/raw", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')


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
 mutate(Collected.Date = as.Date(Collected.Date, format = '%d/%m/%y')) %>%
 mutate(temp = as.numeric(Value)) %>%
 dplyr::arrange(Collected.Date, desc(Site.Ref)) %>%
 filter(Variable.Name == "Water Temp situ(øC) MAX" | 
          Variable.Name == "Water Temp situ(øC) MEAN" | 
          Variable.Name == "Water Temp situ(øC) MIN") %>%
 glimpse()


# save new df ----
#write.csv(df3, paste(dt.dir, "Borewater_Temp.csv", sep='/'))


# p <- ggplot(df3 %>% filter(Site.Ref == "61715045", Variable.Name == "Water Temp situ(øC) MEAN"), aes(x = Collected.Date, y = temp)) +
#   geom_line() +
#  # facet_wrap(~Site.Ref) +
#   theme_classic()
# p

ynames <- levels(df3$Site.name )

for(i in 1:length(levels(df3$Site.name ))) {
  tiff(paste(p.dir, paste(ynames[i], "tiff", sep='.'), sep='/'), width = 3, height = 3.5, units = 'in', pointsize = 6, res = 600)
  print(
    ggplot(df3 %>% filter(Site.name  == ynames[i] & Variable.Name == "Water Temp situ(øC) MEAN"), aes(x = Collected.Date, y = temp)) +
      geom_line() +
    labs(title = ynames[i])
  )
  dev.off()
}


## for monthly averages ----

df <- read.csv(paste(dt.dir, "Borewater_Temp.csv", sep='/')) %>%
  mutate(Collected.Date = as.Date(Collected.Date)) %>%
  mutate_at(month = months(Collected.Date)) %>%
  glimpse()
head(df)
str(df)

df$month <- months(df$Collected.Date)
head(df)
str(df)
