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
source("C:/Users/krshedd/Documents/r/Functions.GCL.R")

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
# Need to combine Chignik + AK Pen/Aleutian Islands and Norton Sound + Kotzebue
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

harvest_tidy <- bind_rows(comm_harvest_tidy, subs_harvest_tidy, sport_harvest_tidy) %>% 
  add_row(Year = 2014:2016, Area = "GOA", Harvest = c(15751, 18969, 22080), Fishery = "Bycatch") %>%  # from NOAA website <https://alaskafisheries.noaa.gov/fisheries-catch-landings?tid=286>
  add_row(Year = 2014:2016, Area = "BSAI", Harvest = c(18098, 25254, 32561), Fishery = "Bycatch") %>%  # from NOAA website <https://alaskafisheries.noaa.gov/fisheries-catch-landings?tid=286>
  replace_na(list(Harvest = 0L)) %>% 
  mutate(Fishery = factor(x = Fishery, levels = c("Subsistence", "Commercial", "Sport", "Bycatch"))) %>% 
  mutate(Area = recode(Area, !!!level_key)) %>% 
  group_by(Year, Fishery, Area) %>% 
  summarise(Harvest = sum(Harvest, na.rm = TRUE)) %>% 
  ungroup()

avg_harvest_tidy <- harvest_tidy %>% 
  group_by(Fishery, Area) %>% 
  summarise(Harvest = mean(Harvest)) %>% 
  ungroup()

avg_harvest_tidy %>% 
  ggplot(aes(x = Area, y = Harvest, fill = Fishery)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Make Wide

avg_harvest_wide <- avg_harvest_tidy %>% 
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
png(filename = "figures/2_harvest_map.png", width = 8.5, height = 6.2, units = "in", res = 300)

par(bg = "black", col.lab = "white", col.axis = "white")

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

legend("topright", legend = levels(avg_harvest_tidy$Fishery), fill = pie.colors, cex = 1.8, bg = "white")

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/1_harvest_map_blank.png", width = 8.5, height = 6.2, units = "in", res = 300)

par(bg = "black", col.lab = "white", col.axis = "white")

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SEAK

#~~~~~~~~~~~~~~~~~~
# Troll
troll_mat <- c(dget(file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15/Estimates objects/AllYearTroll2009_2015_8RG_StratifiedEstimatesStats.txt")[as.character(2014:2015)], 
               list("2016" = dget(file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16/Estimates objects/AllYearTroll2016_8RG_StratifiedEstimatesStats.txt")))

# Make dataframe
troll_df <- bind_rows(
  lapply(troll_mat, function(mix) {
  tmp <- as.data.frame(mix)
  tmp$RG <- rownames(mix)
  tmp} )
, .id = "item")

# Make tidy
troll_tidy <- troll_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "SEAK/TBR") %>% 
  mutate(area = "Southeast") %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(gear = "Troll") %>% 
  mutate(harvest_type = "Traditional")


#~~~~~~~~~~~~~~~~~~
# Sport
sport_mat <- dget(file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16/Estimates objects/AllYearSport2009_2016_8RG_StratifiedEstimatesStats.txt")[as.character(2014:2016)]

# Make dataframe
sport_df <- bind_rows(
  lapply(sport_mat, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
sport_tidy <- sport_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "SEAK/TBR") %>% 
  mutate(area = "Southeast") %>% 
  mutate(fishery = "Sport") %>% 
  mutate(gear = NA) %>% 
  mutate(harvest_type = NA)


#~~~~~~~~~~~~~~~~~~
# Terminal
# Make dataframe
terminal_tidy <- tibble(year = 2014:2016,
                        rho = 1,
                        RG = "SEAK/TBR",
                        area = "Southeast",
                        fishery = "Commercial",
                        gear = NA,
                        harvest_type = "Terminal")


#~~~~~~~~~~~~~~~~~~
# D8&11 Drift
# Hand enter to save time, visually examined and harvest is almost certainly 100% SEAK origin
drift_tidy <- tibble(year = 2014:2016,
                     rho = 1,
                     RG = "SEAK/TBR",
                     area = "Southeast",
                     fishery = "Commercial",
                     gear = "Drift",
                     harvest_type = "Traditional")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copper River gillnet
cr_mat <- dget("V:/Analysis/2_Central/Chinook/Copper River/Mixtures/CopperMixtures 2016-2017/Estimates objects/Annual2013_2017_8RG_StratifiedEstimatesStats.txt")[as.character(2014:2016)]

# Make dataframe
cr_df <- bind_rows(
  lapply(cr_mat, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
cr_tidy <- cr_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG %in% c("NEGulfAK", "CoastalSEAK")) %>% 
  group_by(year) %>% 
  summarise(rho = sum(rho)) %>% 
  mutate(RG = "SEAK/TBR") %>% 
  mutate(area = "Prince William Sound") %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(gear = "Drift") %>% 
  mutate(harvest_type = "Traditional")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Lower Cook Inlet sport
setwd("V:/Analysis/4_Westward/Chinook/CSRI Westward Commercial Harvest 2014-2016/Mixtures/Objects/")
groups10 <- dget(file = "groups10.txt")
groupvec10 <- dget(file = "groupvec10.txt")

#~~~~~~~~
setwd("V:/Analysis/2_Central/Chinook/Lower Cook Inlet/2016/Mixture/Sport Harvest 2014to2016")

catchvec14<-c(554,985,6119,3173)
names(catchvec14)<-paste0(c("E","L","S","W"),14)
catchvec15<-c(2658,1528,9796,5137)
names(catchvec15)<-paste0(c("E","L","S","W"),15)
catchvec16 <- colMeans(rbind(catchvec14 / sum(catchvec14), catchvec15 / sum(catchvec15))) * (mean(c(sum(catchvec14) / 11989, sum(catchvec15) / 19515)) * 20005)  # average catchvec dist from 2014-2015 times the average % of Cook Inlet saltwater (PS) Chinook harvest from SWHS that was in catchvec

LCI_2014 <- CustomCombineBAYESOutput.GCL(groupvec = groupvec10, groupnames = groups10, maindir = "BAYES/Output/", mixvec = c("E14", "L16", "S14", "W14"), ext = "BOT", nchains = 4, burn = 0.5, alpha = 0.1, threshhold = 5e-7, PosteriorOutput = FALSE)
LCI_2014_df <- bind_rows(
  lapply(LCI_2014, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")
LCI_2014_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska")

LCI_2015 <- CustomCombineBAYESOutput.GCL(groupvec = groupvec10, groupnames = groups10, maindir = "BAYES/Output/", mixvec = c("E15", "L16", "S15", "W15"), ext = "BOT", nchains = 4, burn = 0.5, alpha = 0.1, threshhold = 5e-7, PosteriorOutput = FALSE)
LCI_2015_df <- bind_rows(
  lapply(LCI_2015, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")
LCI_2015_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska")

LCI_2016 <- CustomCombineBAYESOutput.GCL(groupvec = groupvec10, groupnames = groups10, maindir = "BAYES/Output/", mixvec = c("E16", "L16", "S16", "W16"), ext = "BOT", nchains = 4, burn = 0.5, alpha = 0.1, threshhold = 5e-7, PosteriorOutput = FALSE)
LCI_2016_df <- bind_rows(
  lapply(LCI_2016, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")
LCI_2016_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska")


Overall2014=StratifiedEstimator.GCL(groupvec=groupvec10, groupnames=groups10, maindir="BAYES/Output", mixvec=c("E14","L16","S14","W14"), catchvec=catchvec14, CVvec=rep(0,length(catchvec14)), newname="Overall14", priorname="", ext="BOT", nchains=4, burn=0.5, alpha=0.1, threshold=5e-7, xlxs=FALSE)
Overall2015=StratifiedEstimator.GCL(groupvec=groupvec10, groupnames=groups10, maindir="BAYES/Output", mixvec=c("E15","L16","S15","W15"), catchvec=catchvec15, CVvec=rep(0,length(catchvec15)), newname="Overall15", priorname="", ext="BOT", nchains=4, burn=0.5, alpha=0.1, threshold=5e-7, xlxs=FALSE)
Overall2016=StratifiedEstimator.GCL(groupvec=groupvec10, groupnames=groups10, maindir="BAYES/Output", mixvec=c("E16","L16","S16","W16"), catchvec=catchvec16, CVvec=rep(0,length(catchvec16)), newname="Overall16", priorname="", ext="BOT", nchains=4, burn=0.5, alpha=0.1, threshold=5e-7, xlxs=FALSE)

# Make dataframe
LCI_df <- bind_rows(
  lapply(list("2014" = Overall2014, "2015" = Overall2015, "2016" = Overall2016), function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
LCI_tidy <- LCI_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska") %>% 
  mutate(RG = "SEAK/TBR") %>% 
  mutate(area = "Cook Inlet") %>% 
  mutate(fishery = "Sport") %>% 
  mutate(gear = NA) %>% 
  mutate(harvest_type = NA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Kodiak commercial
kod_comm_mat <- list("2014" = dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Commercial Harvest 2014-2016/Mixtures/Estimates objects/Final/KMA2014_Annual_Stratified_EstimatesStats.txt"),
                "2015" = dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Commercial Harvest 2014-2016/Mixtures/Estimates objects/Final/KMA2015_Annual_Stratified_EstimatesStats.txt"),
                "2016" = dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Commercial Harvest 2014-2016/Mixtures/Estimates objects/Final/KMA2016_Annual_Stratified_EstimatesStats.txt"))

# Make dataframe
kod_comm_df <- bind_rows(
  lapply(kod_comm_mat, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
kod_comm_tidy <- kod_comm_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska") %>% 
  group_by(year) %>% 
  summarise(rho = sum(rho)) %>% 
  mutate(RG = "SEAK/TBR") %>% 
  mutate(area = "Kodiak") %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(gear = "Seine") %>% 
  mutate(harvest_type = "Traditional")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Kodiak sport
kod_sport_mat <- c(dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Sport Harvest 2014-2016/Mixtures/Estimates objects/KMARS14_EstimatesStats.txt"),
                   dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Sport Harvest 2014-2016/Mixtures/Estimates objects/KMARS15_EstimatesStats.txt"),
                   dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Sport Harvest 2014-2016/Mixtures/Estimates objects/KMARS16_EstimatesStats.txt"))
names(kod_sport_mat) <- 2014:2016

# Make dataframe
kod_sport_df <- bind_rows(
  lapply(kod_sport_mat, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
kod_sport_tidy <- kod_sport_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska") %>% 
  group_by(year) %>% 
  summarise(rho = sum(rho)) %>% 
  mutate(RG = "SEAK/TBR") %>% 
  mutate(area = "Kodiak") %>% 
  mutate(fishery = "Sport") %>% 
  mutate(gear = NA) %>% 
  mutate(harvest_type = NA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## South Pen Chignik
sakpen_mat <- dget("V:/Analysis/4_Westward/Chinook/CSRI Westward Commercial Harvest 2014-2016/Mixtures/Estimates objects/Final/KSPENCHIG2014_EstimatesStats.txt")
names(sakpen_mat) <- 2014

# Make dataframe
sakpen_df <- bind_rows(
  lapply(sakpen_mat, function(mix) {
    tmp <- as.data.frame(mix)
    tmp$RG <- rownames(mix)
    tmp} )
  , .id = "item")

# Make tidy
sakpen_tidy <- sakpen_df %>% 
  rename(year = item, rho = mean) %>% 
  select(year, rho, RG) %>% 
  filter(RG == "Southeast Alaska / Northeast Gulf of Alaska") %>% 
  group_by(year) %>% 
  summarise(rho = sum(rho)) %>% 
  mutate(RG = "SEAK/TBR") %>% 
  mutate(area = "AK Pen") %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(gear = NA) %>% 
  mutate(harvest_type = NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GOA Pollock
# All GOA Pollock from Chuck's NOAA reports
goa_tidy <- tibble(year = 2014:2016,
                   rho = c(0.010 + 0.179, 0.002 + 0.136, 0.020 + 0.150),
                   RG = "SEAK/TBR",
                   area = "GOA",
                   fishery = "Bycatch",
                   gear = NA,
                   harvest_type = NA)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## BSAI Pollock
# All BSAI Pollock from Chuck's NOAA reports
bsai_tidy <- tibble(year = 2014:2016,
                    rho = c(0.000 + 0.014, 0.000 + 0.045, 0.000 + 0.044),
                    RG = "SEAK/TBR",
                    area = "BSAI",
                    fishery = "Bycatch",
                    gear = NA,
                    harvest_type = NA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Bind all stock comp data
rho_tidy <- as.tibble(
  rbind(troll_tidy,
        sport_tidy,
        terminal_tidy,
        drift_tidy,
        cr_tidy,
        LCI_tidy,
        kod_comm_tidy,
        kod_sport_tidy,
        sakpen_tidy,
        goa_tidy,
        bsai_tidy))

rho_tidy <- rho_tidy %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(fishery = factor(x = fishery, levels = levels(harvest_tidy$Fishery))) %>% 
  mutate(area = factor(x = area, levels = levels(harvest_tidy$Area)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Harvest for Stock Comp ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SEAK
# Assume all SEAK terminal is SEAK, regardless of gear class
# Multiply troll stock comp by the spring troll + traditional harvest (not terminal)
# So I'm assuming we have NO information on non-terminal drift, purse, set
seak_comm_harvest <- read_csv(file = "data/harvest/comm/CE002636.csv", skip = 22)


# Assume Terminal is 100% SEAK
terminal_h_tidy <- seak_comm_harvest %>% 
  rename(harvest_type = Harvest) %>% 
  rename(year = Year) %>% 
  group_by(year, harvest_type) %>% 
  summarise(harvest = sum(`N Catch`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(harvest_type == "TERM") %>% 
  mutate(harvest_type = recode(harvest_type, TERM = "Terminal")) %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(area = "Southeast") %>% 
  mutate(gear = NA)

# Non-terminal Troll harvest to multiply by Annual troll stock comp
troll_h_tidy <- seak_comm_harvest %>% 
  rename(harvest_type = Harvest) %>% 
  rename(year = Year) %>% 
  rename(gear = `Gear Class`) %>% 
  filter(gear == "TROLL", harvest_type != "TERM") %>% 
  mutate(gear = recode(gear, TROLL = "Troll")) %>% 
  group_by(year, gear) %>% 
  summarise(harvest = sum(`N Catch`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(area = "Southeast") %>% 
  mutate(harvest_type = "Traditional")
  
# TBR D8&11 Gillnet through SW 29
drift_h_tidy <- seak_comm_harvest %>% 
  rename(harvest_type = Harvest) %>% 
  rename(year = Year) %>% 
  rename(gear = `Gear Class`) %>% 
  filter(gear == "DRIFT", harvest_type == "TRAD", `Time Value` <= 29, `Area Value`%in% c(108, 111)) %>% 
  mutate(gear = recode(gear, DRIFT = "Drift")) %>% 
  group_by(year, gear) %>% 
  summarise(harvest = sum(`N Catch`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(fishery = "Commercial") %>% 
  mutate(area = "Southeast") %>% 
  mutate(harvest_type = "Traditional")

# Sport
sport_h_tidy <- tibble(
  year = 2014:2016,
  gear = NA,
  harvest = unlist(sport_harvest_tidy %>% filter(Area == "Southeast") %>% select(Harvest)),
  fishery = "Sport",
  area = "Southeast",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copper River gillnet
# From Copper River script
catchvec_2014 <- c(1264, 851, 1470, 1210, 1182, 1283, 1281, 822)
catchvec_2015 <- c(1517, 2959, 2460, 3002, 1932, 1882, 1523, 872, 2219, 980)
catchvec_2016 <- c(1367, 1968, 2912, 1116, 988, 904, 624, 303)

cr_h_tidy <- tibble(
  year = 2014:2016,
  gear = "Drift",
  harvest = c(sum(catchvec_2014), sum(catchvec_2015), sum(catchvec_2016)),
  fishery = "Commercial",
  area = "Prince William Sound",
  harvest_type = "Traditional"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Lower Cook Inlet sport
# From Andy's LCI script
catchvec14<-c(554,985,6119,3173)
names(catchvec14)<-paste0(c("E","L","S","W"),14)
catchvec15<-c(2658,1528,9796,5137)
names(catchvec15)<-paste0(c("E","L","S","W"),15)
catchvec16 <- colMeans(rbind(catchvec14 / sum(catchvec14), catchvec15 / sum(catchvec15))) * (mean(c(sum(catchvec14) / 11989, sum(catchvec15) / 19515)) * 20005)  # average catchvec dist from 2014-2015 times the average % of Cook Inlet saltwater (PS) Chinook harvest from SWHS that was in catchvec

LCI_h_tidy <- tibble(
  year = 2014:2016,
  gear = NA,
  harvest = c(sum(catchvec14), sum(catchvec15), sum(catchvec16)),
  fishery = "Sport",
  area = "Cook Inlet",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Kodiak commercial
# From KMA comm report
kod_comm_harvest <- setNames(c(6867, 7447, 6791), nm = 2014:2016)

kod_comm_h_tidy <- tibble(
  year = 2014:2016,
  gear = "Seine",
  harvest = kod_comm_harvest,
  fishery = "Commercial",
  area = "Kodiak",
  harvest_type = "Traditional"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Kodiak sport
# From KMA sport report + SWHS (salt water only)
kod_sport_harvest <- setNames(c(8049, 6709, 9499), nm = 2014:2016)

kod_sport_h_tidy <- tibble(
  year = 2014:2016,
  gear = NA,
  harvest = kod_sport_harvest,
  fishery = "Sport",
  area = "Kodiak",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## South Pen Chignik
setNames(object = 12209, nm = 2014)
harvest_tidy %>% 
  filter(Area == "AK Pen" & Fishery == "Commercial") %>% 
  group_by(Year) %>% 
  summarise(Harvest = sum(Harvest))

sakpen_h_tidy <- tibble(
  year = 2014L,
  gear = NA,
  harvest = 12209,
  fishery = "Commercial",
  area = "AK Pen",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GOA Pollock
goa_pollock_harvest <- setNames(c(10883, 13612, 20882), nm = 2014:2016)

goa_h_tidy <- tibble(
  year = 2014:2016,
  gear = NA,
  harvest = goa_pollock_harvest,
  fishery = "Bycatch",
  area = "GOA",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## BSAI Pollock
bsai_pollock_harvest <- setNames(c(15031, 18329, 21926), nm = 2014:2016)

bsai_h_tidy <- tibble(
  year = 2014:2016,
  gear = NA,
  harvest = bsai_pollock_harvest,
  fishery = "Bycatch",
  area = "BSAI",
  harvest_type = NA
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join with harvest tidy
rho_h_tidy <- bind_rows(
  troll_h_tidy,
  sport_h_tidy,
  terminal_h_tidy,
  drift_h_tidy,
  cr_h_tidy,
  LCI_h_tidy,
  kod_comm_h_tidy,
  kod_sport_h_tidy,
  sakpen_h_tidy,
  goa_h_tidy,
  bsai_h_tidy
)

# Join rho and paired harvest
rho_h_join <- rho_h_tidy %>% 
  mutate(fishery = factor(x = fishery, levels = levels(harvest_tidy$Fishery))) %>% 
  mutate(area = factor(x = area, levels = levels(harvest_tidy$Area))) %>% 
  left_join(rho_tidy)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Map with stock comp harvest ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join paired harvest with all harvest, take average across 2014-2016
avg_rho_h_join <- rho_h_join %>% 
  filter(area != "AK Pen") %>%  # drop AK Pen, only 1 year of stock comp, 2014
  group_by(year, fishery, area) %>%  # summarize harvest across gear and harvest types
  summarise(harvest = sum(harvest)) %>% 
  group_by(fishery, area) %>%  # average harvest across years for each fishery and area
  summarize(avg_harvest = mean(harvest)) %>% 
  ungroup() %>% 
  rename(Fishery = fishery, Area = area) %>% 
  right_join(avg_harvest_tidy) %>%  # join with all harvest
  replace(is.na(.), 0) %>% 
  mutate(harvest_no_stock = Harvest - avg_harvest)  # how much harvest doesn't have paired stock-specific data

# Wide stock harvest
rho_h_wide <- avg_rho_h_join %>% 
  select(Fishery, Area, avg_harvest) %>% 
  spread(Fishery, avg_harvest) %>% 
  replace(is.na(.), 0)

# Wide no stock harvest
no_rho_h_wide <- avg_rho_h_join %>% 
  select(Fishery, Area, harvest_no_stock) %>% 
  spread(Fishery, harvest_no_stock) %>% 
  replace(is.na(.), 0)

rho_chinookharvest <- bind_cols(
  rho_h_wide[, 1:2], no_rho_h_wide[, 2], 
  rho_h_wide[, 3], no_rho_h_wide[, 3], 
  rho_h_wide[, 4], no_rho_h_wide[, 4],
  rho_h_wide[, 5], no_rho_h_wide[, 5])
rho_chinookharvest$Total = rowSums(rho_chinookharvest[, -1])

names(rho_chinookharvest) <- c("Area", "rho_Subsistence", "Subsistence", "rho_Commercial", "Commercial", "rho_Sport", "Sport", "rho_Bycatch", "Bycatch", "Total")

# Save harvest data
setwd("V:/Presentations/Science/AFS/AFS-Western-Anchorage-2018/Shedd SEAK Chinook/")
write_csv(x = rho_chinookharvest, path = "data/rho_chinookharvest.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/3_harvest_map_stockcomp.png", width = 8.5, height = 6.2, units = "in", res = 300)

par(bg = "black", col.lab = "white", col.axis = "white")

plotMap(land, col = "white", bg = "grey80", plt = c(0.07, .99, 0.09, 0.99), cex.lab = 1.5, cex.axis = 1.5)
addLines(polys = rivers, col = "grey80", lwd = 2)
addLines(polys = borders, col = "black", lwd = 2)
addPolys(polys = StatAreasPBS.shp, col = "grey80", border = "black")
addPolys(polys = DistrictsPBS.shp, col = "grey80", border = "black")

max.rad <- 3
pie.colors <- brewer.pal(n = 4, name = "Set3")[c(1, 3, 2, 4)]
pie.colors <- c("black", pie.colors[1], "black", pie.colors[2], "black", pie.colors[3], "black", pie.colors[4])

add.pie(z = as.numeric(rho_chinookharvest[1, 2:9]), x = -140, y = 56, radius = sqrt(as.numeric(rho_chinookharvest[1, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[2, 2:9]), x = -146, y = 59.75, radius = sqrt(as.numeric(rho_chinookharvest[2, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[3, 2:9]), x = -152, y = 60, radius = sqrt(as.numeric(rho_chinookharvest[3, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[4, 2:9]), x = -152, y = 57, radius = sqrt(as.numeric(rho_chinookharvest[4, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[5, 2:9]), x = -161, y = 54.5, radius = sqrt(as.numeric(rho_chinookharvest[5, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[6, 2:9]), x = -159, y = 58, radius = sqrt(as.numeric(rho_chinookharvest[6, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[7, 2:9]), x = -157, y = 61.5, radius = sqrt(as.numeric(rho_chinookharvest[7, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[8, 2:9]), x = -158, y = 64.5, radius = sqrt(as.numeric(rho_chinookharvest[8, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[9, 2:9]), x = -165, y = 64, radius = sqrt(as.numeric(rho_chinookharvest[9, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[10, 2:9]), x = -149, y = 55, radius = sqrt(as.numeric(rho_chinookharvest[10, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(rho_chinookharvest[11, 2:9]), x = -168, y = 57, radius = sqrt(as.numeric(rho_chinookharvest[11, 10]/max(rho_chinookharvest[, 10]))) * max.rad, labels = NA, col = pie.colors)

legend("topright", legend = c(levels(avg_harvest_tidy$Fishery), "Genetics"), fill = pie.colors[c(2, 4, 6, 8, 1)], cex = 1.8, bg = "white")

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Map with SEAK stock comp harvest ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join paired harvest with all harvest, take average across 2014-2016
avg_seak_rho_h_join <- rho_h_join %>% 
  filter(area != "AK Pen") %>%  # drop AK Pen, only 1 year of stock comp, 2014
  mutate(seak_harvest = rho * harvest) %>%  # how many seak chinook?
  group_by(year, fishery, area) %>%  # summarize harvest across gear and harvest types
  summarise(harvest = sum(harvest), seak_harvest = sum(seak_harvest)) %>% 
  group_by(fishery, area) %>%  # average harvest across years for each fishery and area
  summarize(avg_harvest = mean(harvest), avg_seak_harvest = mean(seak_harvest)) %>% 
  ungroup() %>% 
  rename(Fishery = fishery, Area = area) %>% 
  right_join(avg_harvest_tidy) %>%  # join with all harvest
  replace(is.na(.), 0) %>% 
  mutate(harvest_no_stock = Harvest - avg_harvest) %>%  # how much harvest doesn't have paired stock-specific data
  mutate(harvest_not_seak = avg_harvest - avg_seak_harvest) %>% 
  select(Fishery, Area, avg_seak_harvest, harvest_not_seak, harvest_no_stock, Harvest)
save_objects(objects = "avg_seak_rho_h_join", path = "data/")

# Wide seak stock harvest
seak_rho_h_wide <- avg_seak_rho_h_join %>% 
  select(Fishery, Area, avg_seak_harvest) %>% 
  spread(Fishery, avg_seak_harvest) %>% 
  replace(is.na(.), 0)

# Wide not seak stock harvest
not_seak_rho_h_wide <- avg_seak_rho_h_join %>% 
  select(Fishery, Area, harvest_not_seak) %>% 
  spread(Fishery, harvest_not_seak) %>% 
  replace(is.na(.), 0)

# Wide no stock stock harvest
no_rho_h_wide <- avg_seak_rho_h_join %>% 
  select(Fishery, Area, harvest_no_stock) %>% 
  spread(Fishery, harvest_no_stock) %>% 
  replace(is.na(.), 0)


seak_rho_chinookharvest <- bind_cols(
  seak_rho_h_wide[, 1:2], not_seak_rho_h_wide[, 2], no_rho_h_wide[, 2], 
  seak_rho_h_wide[, 3], not_seak_rho_h_wide[, 3], no_rho_h_wide[, 3], 
  seak_rho_h_wide[, 4], not_seak_rho_h_wide[, 4], no_rho_h_wide[, 4],
  seak_rho_h_wide[, 5], not_seak_rho_h_wide[, 5], no_rho_h_wide[, 5])
seak_rho_chinookharvest$Total = rowSums(seak_rho_chinookharvest[, -1])

seak_rho_chinookharvest$Total == chinookharvest$Total

names(seak_rho_chinookharvest) <- c("Area", "seak_rho_Subsistence", "not_seak_rho_Subsistence", "Subsistence", "seak_rho_Commercial", "not_seak_rho_Commercial", "Commercial", "seak_rho_Sport", "not_seak_rho_Sport", "Sport", "seak_rho_Bycatch", "not_seak_rho_Bycatch", "Bycatch", "Total")

# Save harvest data
setwd("V:/Presentations/Science/AFS/AFS-Western-Anchorage-2018/Shedd SEAK Chinook/")
write_csv(x = seak_rho_chinookharvest, path = "data/seak_rho_chinookharvest.csv")

# Randy's SEAK color (hatchery + wild)
seak_color <- rgb(red = 155, green = 187, blue = 89, maxColorValue = 255)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/4_harvest_map_stockcomp_seak.png", width = 8.5, height = 6.2, units = "in", res = 300)

par(bg = "black", col.lab = "white", col.axis = "white")

plotMap(land, col = "white", bg = "grey80", plt = c(0.07, .99, 0.09, 0.99), cex.lab = 1.5, cex.axis = 1.5)
addLines(polys = rivers, col = "grey80", lwd = 2)
addLines(polys = borders, col = "black", lwd = 2)
addPolys(polys = StatAreasPBS.shp, col = "grey80", border = "black")
addPolys(polys = DistrictsPBS.shp, col = "grey80", border = "black")

max.rad <- 3
pie.colors <- brewer.pal(n = 4, name = "Set3")[c(1, 3, 2, 4)]
pie.colors <- c(seak_color, "black", pie.colors[1], seak_color, "black", pie.colors[2], seak_color, "black", pie.colors[3], seak_color, "black", pie.colors[4])

add.pie(z = as.numeric(seak_rho_chinookharvest[1, 2:13]), x = -140, y = 56, radius = sqrt(as.numeric(seak_rho_chinookharvest[1, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[2, 2:13]), x = -146, y = 59.75, radius = sqrt(as.numeric(seak_rho_chinookharvest[2, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[3, 2:13]), x = -152, y = 60, radius = sqrt(as.numeric(seak_rho_chinookharvest[3, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[4, 2:13]), x = -152, y = 57, radius = sqrt(as.numeric(seak_rho_chinookharvest[4, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[5, 2:13]), x = -161, y = 54.5, radius = sqrt(as.numeric(seak_rho_chinookharvest[5, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[6, 2:13]), x = -159, y = 58, radius = sqrt(as.numeric(seak_rho_chinookharvest[6, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[7, 2:13]), x = -157, y = 61.5, radius = sqrt(as.numeric(seak_rho_chinookharvest[7, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[8, 2:13]), x = -158, y = 64.5, radius = sqrt(as.numeric(seak_rho_chinookharvest[8, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[9, 2:13]), x = -165, y = 64, radius = sqrt(as.numeric(seak_rho_chinookharvest[9, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[10, 2:13]), x = -149, y = 55, radius = sqrt(as.numeric(seak_rho_chinookharvest[10, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)
add.pie(z = as.numeric(seak_rho_chinookharvest[11, 2:13]), x = -168, y = 57, radius = sqrt(as.numeric(seak_rho_chinookharvest[11, 14]/max(seak_rho_chinookharvest[, 14]))) * max.rad, labels = NA, col = pie.colors)

legend("topright", legend = c(levels(avg_harvest_tidy$Fishery), "Genetics", "SEAK"), fill = c(brewer.pal(n = 4, name = "Set3")[c(1, 3, 2, 4)], "black", seak_color), cex = 1.8, bg = "white")

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(filename = "figures/5_harvest_barplot_stockcomp_seak.png", width = 8.5, height = 6.2, units = "in", res = 300)

avg_seak_rho_h_join %>% 
  mutate(avg_seak_harvest = avg_seak_harvest / 1000) %>% 
  ggplot(aes(x = Area, y = avg_seak_harvest, fill = Fishery)) +
  geom_bar(position = 'stack', stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(size = 20, colour = "white"),
        plot.background = element_rect(fill = "black", colour = "black"),
        axis.text = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.line = element_line(colour = "white"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = "grey30")) +
  ylab("Average Harvest of SEAK\nChinook Salmon (1000's)") +
  scale_fill_manual(values = brewer.pal(n = 4, name = "Set3")[c(1, 3, 2, 4)])

dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

avg_seak_rho_h_join %>% 
  mutate(freq = avg_seak_harvest / sum(avg_seak_harvest) * 100) %>% 
  arrange(desc(freq))

save.image("GOA_SEAK_chinook.RData")
