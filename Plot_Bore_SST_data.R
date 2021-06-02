### Plot bore water and SST ----


# libraries ----
library(dplyr)
library(ggplot2)
library(lubridate)

rm(list=ls())

# Directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
p.dir <- paste(w.dir, "plots", sep = '/')
bore.dir <- paste(w.dir, "bore_temp", sep='/')
sst.dir <- paste(w.dir, "SST_Satellite", sep='/')

# read bore temp data ----
b.temp <- read.csv(paste(bore.dir, "data/tidy", "Bore_Temp_summary.csv", sep='/')) %>%
  mutate_at("Site.name", as.factor) %>%
  mutate(Site.name = fct_recode(Site.name, "Mindarie" = "Artesian Monitoring - AM20B",
                                "Mindarie" = "Artesian Monitoring - AM23B",
                                "Mindarie" = "Artesian Monitoring - AM23C", 
                                "Marmion" = "Artesian Monitoring - AM27C",
                                "Marmion" = "Artesian Monitoring - AM27D",
                                "Marmion"= "Artesian Monitoring - AM27E",
                                "Two Rocks"= "Artesian Monitoring - AM8B",
                                "Cervantes" = "Lake Thetis - TH1B",
                                "Cervantes" = "Lake Thetis - TH1C", 
                                "Cervantes" = "Lake Thetis - TH2C"
                                )) %>%
  mutate(date = lubridate::as_date(date2)) %>%
  mutate(month = lubridate::month(date)) %>%
  mutate_at("month", as.factor) %>%
  mutate(month = fct_recode(month, 'Jan'='1', 'Feb'='2', 'Mar'='3', 'Apr'='4', 'May'='5', 'Jun'='6', 'Jul'='7', 'Aug'='8', 
                            'Sep'='9', 'Oct'='10', 'Nov'='11', 'Dec'='12')) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(month.year = as.factor(paste(year, month, sep='.'))) %>%
  filter(Site.name == "Mindarie"  | Site.name == "Marmion" | Site.name == "Two Rocks") %>%
  group_by(Site.name, month.year) %>%
  summarise(bore.temp = mean(av.temp)) %>%
  rename(Location = Site.name) %>%
  as.data.frame() %>%
  glimpse()

b.temp <- droplevels(b.temp)
summary(b.temp)
str(b.temp)
levels(b.temp$month.year)
# reorder factor levels --
b.temp$month.year <- factor(b.temp$month.year, levels(b.temp$month.year)[c(4,3,1,7,6,5,2,9,8,10)])
levels(b.temp$month.year)


# read SST data ----
sst <- read.csv(paste(sst.dir, "data/tidy", 'SST.monthly.av.all_locations.csv', sep = '/')) %>%
  mutate(date = lubridate::as_date(time)) %>%
  mutate_at(c("month.year", "Location"), as.factor) %>%
  rename(sst = av.temp) %>%
  glimpse()

summary(sst)
str(sst)
levels(sst$month.year)
# reorder factor levels --
sst$month.year <- factor(sst$month.year, levels(sst$month.year)[c(5,4,8,1,9,7,6,2,12,11,10,3,17,16,20,13,21,19,18,14,24,23,22,15)])
levels(sst$month.year)


## combine bore temp and sst ----
sst.bore <- sst %>%
  select(c(month.year, time, sst, Location, date)) %>%
  full_join(b.temp, by = c('Location', 'month.year')) %>%
  pivot_longer(cols = c(sst, bore.temp), names_to = "temp.source", values_to = "monthly.temp.av") %>%
  glimpse()

# check level order form plotting --
levels(sst.bore$month.year)
levels(sst.bore$Location)
sst.bore$Location <- factor(sst.bore$Location, levels(sst.bore$Location)[c(3,2,1)])
levels(sst.bore$Location)

### PLOT ----
theme_set(theme_bw())

months.to.remove <- c("2019.Jan", "2019.Feb", "2019.Mar", "2020.Jun", "2020.Jul", "2020.Aug", "2020.Sep", "2020.Oct", "2020.Nov", "2020.Dec")

p <- ggplot(sst.bore %>% filter(!month.year %in% months.to.remove), aes(x = month.year, y = monthly.temp.av, color = temp.source)) +
  geom_point() +
  facet_wrap(~ Location) +
  labs(x = "Month - year", y = "Average monthly temperature (C)") +
  scale_color_manual(values = c("#999999", "#E69F00"),
                       name = "Source of\ntemperature",
                       labels = c("Bore temperature", "SST")) +
  theme(axis.text = element_text(size = 6, angle = 90))
p

# save plot --
#ggsave(filename = "SST_bore_av_monthly_temp.png", plot = p, device = "png", path = p.dir)
