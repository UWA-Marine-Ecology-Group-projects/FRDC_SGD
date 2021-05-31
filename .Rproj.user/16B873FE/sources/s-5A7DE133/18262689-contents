library(dplyr)

dirx <- "G:/My Drive/meg_projects/Projects_WRL/Project_WRL_freshwater/Data"

df <- read.csv(paste(dirx, "DwerBoreVariableStats.csv", sep ='/'))
head(df)
str(df)

df <- df %>% dplyr::mutate_at(c('Site.Ref', 'Site.name', 'Site.short.name', 'Site.subtype', 'Variable'), as.factor) %>%
  glimpse()

sites <- read.csv(paste(dirx, "Bore_IDs - Sheet1.csv", sep ='/')) %>%
  mutate_at(c('Site.name'), as.factor) %>%
  glimpse()
length(sites$Location) # 15 sites selected
sitesv <- levels(sites$Site.name)
sitesv


df2 <- df[df$Site.name %in% sitesv,] %>%
  glimpse() # 562 rows left

levels(df$Variable)

df2 <- df2[df2$Variable %in% "Temperature (in situ)",] %>%
  glimpse() # 11 rows left, only 11 sites with Temp in situ

df2

write.csv(df2, paste(dirx, "Freshwater_sites_Temperature.csv", sep = '/'))
