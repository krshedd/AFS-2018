#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script will coalate stock-specific harvest data for SEAK Chinook from
# 2014-2016 for my Western Division AFS talk in Anchorage on May 22, 2018.
# I will be gathering genetic data from:
# SEAK troll
# SEAK sport
# SEAK gillnet
# Copper River gillnet
# Lower Cook Inlet sport
# Kodiak commercial
# Kodiak sport
# South Peninsula commercial
# GOA trawl bycatch
# BSAI trawl bycatch
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("V:/Presentations/Science/AFS/AFS-Western-Anchorage-2018/Shedd SEAK Chinook/")

library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Commercial
comm_harvest <- read_csv(file = "data/harvest/comm/Gross Earnings Salmon Summary By AREA.csv")
comm_regions <- unique(comm_harvest$Region)
comm_areas <- unique(comm_harvest$Area)

# 2007-2011 by Area Average
comm_harvest %>% 
  # replace_na(replace = list(`Number Of Fish` = 0)) %>%  # depends if we want average harvest including years when fishing did not occur
  mutate(Region = factor(x = Region, levels = c("Southeast", "Central", "AYK", "Westward"))) %>%  # order Region
  mutate(Area = factor(x = Area, levels = c("Southeast", "Prince William Sound", "Cook Inlet", "Kodiak", "Chignik", "AK Pen/Aleutian Islands", "Bristol Bay", "Kuskokwim", "Yukon", "Norton Sound", "Kotzebue"))) %>%  # order Area
  filter(`Species Code` == "Chinook", Year %in% 2007:2011) %>%  # kings from 2014-2015
  rename(Harvest = `Number Of Fish`) %>%  # more intuitive name
  group_by(Area) %>%  # group across years
  summarise(Harvest = mean(Harvest, na.rm = TRUE))  # mean harvest

# 2014-2016 by Area
comm_harvest_tidy <- comm_harvest %>% 
  # replace_na(replace = list(`Number Of Fish` = 0)) %>%  # depends if we want average harvest including when fishing did not occur
  mutate(Region = factor(x = Region, levels = c("Southeast", "Central", "AYK", "Westward"))) %>%  # order Region
  mutate(Area = factor(x = Area, levels = c("Southeast", "Prince William Sound", "Cook Inlet", "Kodiak", "Chignik", "AK Pen/Aleutian Islands", "Bristol Bay", "Kuskokwim", "Yukon", "Norton Sound", "Kotzebue"))) %>%  # order Area
  filter(`Species Code` == "Chinook", Year %in% 2014:2016) %>%  # kings from 2014-2016
  rename(Harvest = `Number Of Fish`) %>%  # more intuitive name
  select(Year, Area, Harvest) %>%  # drop unecessary variables
  mutate(Fishery = "Commercial")  # add Fishery as variable
  # spread(Area, Harvest)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subsistence
subs_harvest <- read_csv(file = "data/harvest/subsistence/subsistence_harvest_by_region.csv")
all(unique(subs_harvest$District) %in% comm_areas)  # are all the subsistence "District" values the same as commercial areas?
all(unique(subs_harvest$Region) %in% comm_regions)  # are all the subsistence "Region" values the same as commercial regions?

# 2014-2016 by Area
subs_harvest_tidy <- subs_harvest %>% 
  select(-Issued, -Returned, `Total Salmon`) %>%  # drop unnecessary variables
  gather(`Species Code`, Harvest, -Year, -Region, -District, -`Fishery Name`) %>%  # make tidy
  mutate(Region = factor(x = Region, levels = c("Southeast", "Central", "AYK", "Westward"))) %>%  # order Region
  mutate(Area = factor(x = District, levels = c("Southeast", "Prince William Sound", "Cook Inlet", "Kodiak", "Chignik", "AK Pen/Aleutian Islands", "Bristol Bay", "Kuskokwim", "Yukon", "Norton Sound", "Kotzebue"))) %>%  # order Area
  filter(`Species Code` == "Chinook", Year %in% 2014:2016) %>%  # kings from 2014-2016
  group_by(Year, Area, `Species Code`) %>%  # summarise over "Fishery Name" 
  summarize(Harvest = sum(Harvest)) %>% 
  ungroup() %>% 
  select(Year, Area, Harvest) %>%  # drop unecessary variables
  mutate(Fishery = "Subsistence")  # add Fishery as variable
  # spread(Area, Harvest)  # wide

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport
sport_harvest <- read_csv(file = "data/harvest/sport/swhs_est_20180514145052.csv")
sport_regions <- unique(sport_harvest$Region)
sport_areas <- unique(sport_harvest$Area)

# Convert sport areas to comm areas
sport_to_comm_area <- read_csv(file = "data/harvest/sport_comm_region_key.csv")  # note that Bristol Bay and the AK Pen are confounded

# Read in fine scale Bristol Bay to parse out AK Pen
bb_sport_14_16 <- read_csv(file = "data/harvest/sport/swhs_est_20180514161527_R_2014_2106.csv")
bb_sport_14_16_mod <- bb_sport_14_16 %>% 
  group_by(Year, Area) %>%  # summarise over fishing areas
  summarise(Harvest_mod = sum(KS)) %>% 
  ungroup() %>% 
  filter(Area == "AK Pen/Aleutian Islands")  # filter for AK Pen

# Need to add negative fish for Bristol Bay
bb_sport_14_16_mod <- bb_sport_14_16_mod %>% 
  add_row(Year = 2014:2016, Area = "Bristol Bay", Harvest_mod = -bb_sport_14_16_mod$Harvest_mod)

# 2014-2016 by Area
sport_harvest_tidy <- sport_harvest %>% 
  gather(Year, Harvest, -Area_letter, -Area, -Region) %>%  # make tidy (tall)
  left_join(sport_to_comm_area, by = c("Area" = "sport_area")) %>%  # add comm_area
  mutate(Year = as.integer(Year)) %>%  # make Year an integer for joining
  filter(Year %in% 2014:2016) %>%  # filter for 2014-2016
  group_by(Year, comm_area) %>%  # summarise across sport Areas
  summarise(Harvest = sum(Harvest)) %>% 
  ungroup() %>% 
  rename(Area = comm_area) %>%  # rename
  full_join(bb_sport_14_16_mod, by = c("Year", "Area")) %>%  # join with Area R data in order to remove fish from Bristol Bay and add to AK Pen/Aleutian Islands
  replace_na(list(Harvest = 0L, Harvest_mod = 0L)) %>%  # replace NA with 0 (integer) for summing
  mutate(Harvest = Harvest + Harvest_mod) %>%  # subtract BB fish and add AK Pen fish
  select(-Harvest_mod) %>%  # drop extra variable
  mutate(Area = factor(x = Area, levels = c("Southeast", "Prince William Sound", "Cook Inlet", "Kodiak", "Chignik", "AK Pen/Aleutian Islands", "Bristol Bay", "Kuskokwim", "Yukon", "Norton Sound", "Kotzebue"))) %>%  # order Area
  mutate(Fishery = "Sport")  # add Fishery as variable
  # spread(Area, Harvest)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## All State Harvest
harvest_tidy <- bind_rows(comm_harvest_tidy, subs_harvest_tidy, sport_harvest_tidy) %>% 
  replace_na(list(Harvest = 0L)) %>% 
  mutate(Fishery = factor(x = Fishery, levels = c("Subsistence", "Commercial", "Sport")))

avg_harvest_tidy <- harvest_tidy %>% 
  group_by(Fishery, Area) %>% 
  summarise(Harvest = mean(Harvest))

avg_harvest_tidy %>% 
  ggplot(aes(x = Area, y = Harvest, fill = Fishery)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

avg_harvest_wide <- avg_harvest_tidy %>% 
  spread(Fishery, Harvest)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## All State Harvest + Bycatch
harvest_tidy <- bind_rows(comm_harvest_tidy, subs_harvest_tidy, sport_harvest_tidy) %>% 
  add_row(Year = 2014:2016, Area = "GOA", Harvest = c(15751, 18969, 22080), Fishery = "Bycatch") %>%  # from NOAA website <https://alaskafisheries.noaa.gov/fisheries-catch-landings?tid=286>
  add_row(Year = 2014:2016, Area = "BSAI", Harvest = c(18098, 25254, 32561), Fishery = "Bycatch") %>%  # from NOAA website <https://alaskafisheries.noaa.gov/fisheries-catch-landings?tid=286>
  replace_na(list(Harvest = 0L)) %>% 
  mutate(Fishery = factor(x = Fishery, levels = c("Subsistence", "Commercial", "Sport", "Bycatch")))

avg_harvest_tidy <- harvest_tidy %>% 
  group_by(Fishery, Area) %>% 
  summarise(Harvest = mean(Harvest)) %>% 
  ungroup()

avg_harvest_tidy %>% 
  ggplot(aes(x = Area, y = Harvest, fill = Fishery)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Need to combine Chignik + AK Pen/Aleutian Islands and Norton Sound + Kotzebue
level_key = list(Southeast = "Southeast",
                 `Prince William Sound` = "Prince William Sound",
                 `Cook Inlet` = "Cook Inlet",
                 Kodiak = "Kodiak",
                 Chignik = "AK Pen",
                 `AK Pen/Aleutian Islands` = "AK Pen",
                 `Bristol Bay` = "Bristol Bay",
                 Kuskokwim = "Kuskokwim", 
                 Yukon = "Yukon",
                 `Norton Sound` = "Norton Sound",
                 Kotzebue = "Norton Sound",
                 GOA = "GOA",
                 BSAI = "BSAI")

# Summarize over new groups, make wide for plotting
avg_harvest_wide <- avg_harvest_tidy %>% 
  mutate(Area = recode(Area, !!!level_key)) %>% 
  group_by(Fishery, Area) %>% 
  summarise(Harvest = sum(Harvest)) %>% 
  ungroup() %>% 
  spread(Fishery, Harvest)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Maps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
while(!require(maps)) {install.packages("maps")}
while(!require(mapdata)) {install.packages("mapdata")}
while(!require(maptools)) {install.packages("maptools")}
while(!require(mapplots)) {install.packages("mapplots")}
while(!require(GISTools)) {install.packages("GISTools")}
while(!require(rgeos)) {install.packages("rgeos")}
while(!require(sp)) {install.packages("sp")}
while(!require(RColorBrewer)) {install.packages("RColorBrewer")}
while(!require(plotly)) {install.packages("plotly")}
while(!require(PBSmapping)) {install.packages("PBSmapping")}
while(!require(devEMF)) {install.packages("devEMF")}


setwd("V:/Analysis/1_SEAK/Sockeye/Mixture/Harvest Data")

lat <- c(52, 68)
long <- c(185, 230)

land <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/gshhs_h.b", xlim = long, ylim = lat, maxLevel = 4, useWest = TRUE)

rivers <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_rivers_f.b", xlim = long, ylim = lat, useWest = TRUE)

borders <- importGSHHS("C:/Users/krshedd/Documents/R/win-library/3.3/PBSmapping/wdb_borders_f.b", xlim = long, ylim = lat, useWest = TRUE)


# Read in dsistrict shapefile
StatAreasPBS.shp <- importShapefile(fn = "GIS Data/Stat Area/pvs_stat.shp")


# Read in dsistrict shapefile
DistrictsPBS.shp <- importShapefile(fn = "GIS Data/District/pvs_dist.shp")
str(DistrictsPBS.shp)

#
chinookharvest <- avg_harvest_wide %>% 
  replace(is.na(.), 0)
str(chinookharvest)
chinookharvest$Total <- rowSums(chinookharvest[, -1], na.rm = TRUE)

chinookharvest %>% 
  mutate(Total = round(Total / 1000)) %>% 
  select(Area, Total)

# Save harvest data
setwd("V:/Presentations/Science/AFS/AFS-Western-Anchorage-2018/Shedd SEAK Chinook/")
write_csv(x = chinookharvest, path = "data/chinookharvest.csv")

# Save map figure
dir.create("figures")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/harvest_map.png", width = 8.5, height = 6.2, units = "in", res = 300)

plotMap(land, col = "white", bg = "grey80", plt = c(0.07, .99, 0.09, 0.99), cex.lab = 1.5, cex.axis = 1.5)
addLines(polys = rivers, col = "grey80", lwd = 2)
addLines(polys = borders, col = "black", lwd = 2)
addPolys(polys = StatAreasPBS.shp, col = "grey80", border = "black")
addPolys(polys = DistrictsPBS.shp, col = "grey80", border = "black")

max.rad <- 3
pie.colors <- c("chocolate", "cornflowerblue", "beige", "green")
pie.colors <- brewer.pal(n = 4, name = "Set3")[c(1, 3, 2, 4)]


add.pie(z = as.numeric(chinookharvest[1, 2:5]), x = -140, y = 56, radius = sqrt(as.numeric(chinookharvest[1, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[2, 2:5]), x = -146, y = 59.75, radius = sqrt(as.numeric(chinookharvest[2, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[3, 2:5]), x = -152, y = 60, radius = sqrt(as.numeric(chinookharvest[3, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[4, 2:5]), x = -152, y = 57, radius = sqrt(as.numeric(chinookharvest[4, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[5, 2:5]), x = -161, y = 54.5, radius = sqrt(as.numeric(chinookharvest[5, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[6, 2:5]), x = -159, y = 58, radius = sqrt(as.numeric(chinookharvest[6, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[7, 2:5]), x = -157, y = 61.5, radius = sqrt(as.numeric(chinookharvest[7, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[8, 2:5]), x = -158, y = 64.5, radius = sqrt(as.numeric(chinookharvest[8, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[9, 2:5]), x = -165, y = 64, radius = sqrt(as.numeric(chinookharvest[9, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[10, 2:5]), x = -149, y = 55, radius = sqrt(as.numeric(chinookharvest[10, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(chinookharvest[11, 2:5]), x = -168, y = 57, radius = sqrt(as.numeric(chinookharvest[11, 6]/max(chinookharvest[, 6]))) * max.rad, labels = NA, col = pie.colors)

legend("topright", legend = levels(avg_harvest_tidy$Fishery), fill = pie.colors, cex = 2)

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/harvest_map_blank.png", width = 8.5, height = 6.2, units = "in", res = 300)

plotMap(land, col = "white", bg = "grey80", plt = c(0.07, .99, 0.09, 0.99), cex.lab = 1.5, cex.axis = 1.5)
addLines(polys = rivers, col = "grey80", lwd = 2)
addLines(polys = borders, col = "black", lwd = 2)
addPolys(polys = StatAreasPBS.shp, col = "grey80", border = "black")
addPolys(polys = DistrictsPBS.shp, col = "grey80", border = "black")

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in Stock Composition Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("C:/Users/krshedd/Documents/r/Functions.GCL.R")

# SEAK
# Troll
dget(file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15/Estimates objects/AllYearTroll2009_2015_8RG_StratifiedEstimatesStats.txt")
dget(file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16/Estimates objects/AllYearTroll2016_8RG_StratifiedEstimatesStats.txt")
