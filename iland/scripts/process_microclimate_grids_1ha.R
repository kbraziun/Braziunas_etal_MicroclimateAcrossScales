#####
#
## Process microclimate temperature buffering grids
#
#####

### load libraries
library(tidyverse)
library(terra)
library(sf)

###
# 1. arguments
###

# command line arguments
args = commandArgs(TRUE)
micro_year = as.integer(args[1])
path = as.character(args[2])

###
# 2. load data
###

### rid, stand, forest grids
id.rast <- rast("processed_data/iland_simulation_prep/stand_id_raster.tif")
rid.grid <- rast("iland/gis/objectid.asc")
crs(rid.grid) <- "epsg:31468"

# read in forest mask for bgd
for.mask <- rast("iland/gis/standgrid.asc")
NAflag(for.mask) <- -2
crs(for.mask) <- "epsg:31468"

# read in forest types for bgd
for.types <- rast("data/BGD/forest_types2020_iland.tif") %>%
  disagg(fact=10)
crs(for.types) <- "epsg:31468"

# lookup table
for.lookup <- read.csv("data/BGD/forest_types2020_iland_lookup.csv") %>%
  mutate(forest_type_eng = ifelse(forest_type_eng=="Spruce-fir","Spruce-fir-beech",forest_type_eng))

### microclimate buffer grids
iland.buff <- c(rast(list.files(paste0(path,"year_",micro_year,"/"), full.names=TRUE, pattern="Buffer_[0-9]*.asc")))
crs(iland.buff) <- "epsg:31468"

###
# 3. assign forest type and season
###

### prep for matching
rid.match <- rid.grid %>%
  disagg(fact=10)

# names lookup
buff.lookup <- data.frame(name=names(iland.buff)) %>%
  mutate(name2=name) %>%
  separate(name2, into=c("var","month"), sep="_") %>%
  mutate(month = as.integer(month))

buff.stack <- c(rid.match,id.rast,crop(iland.buff,id.rast))

### export to dataframe, add mean
buff.df <- as.data.frame(buff.stack) %>%
  filter(!is.na(lyr.1)) %>%
  pivot_longer(c(MaxTBuffer_01:MinTBuffer_12)) %>%
  left_join(buff.lookup, by="name") %>%
  dplyr::select(-name) %>%
  # group by rid, reduce output size
  group_by(objectid, var, month) %>%
  summarise(value=mean(value)) %>%
  pivot_wider(names_from="var",values_from="value") %>%
  # rename, add mean, add year
  rename(buff_maxT = MaxTBuffer,
         buff_minT = MinTBuffer,
         rid = objectid) %>%
  mutate(buff_meanT = (buff_minT+buff_maxT)/2,
         year=micro_year)

# assign forest type and season to buffering values
buff.summary <- c(rid.match, for.types) %>%
  terra::mask(for.mask) %>%
  as.data.frame() %>%
  rename(rid=objectid) %>%
  # aggregate to rid, forest types as 1 ha resolution
  group_by(rid) %>%
  summarise(forest_types2020_iland = mean(forest_types2020_iland)) %>%
  mutate(landscape="landscape") %>%
  right_join(buff.df, by=c("rid")) %>%
  left_join(for.lookup, by="forest_types2020_iland") %>%
  mutate(season = ifelse(month %in% c(12,1,2),"winter",
                         ifelse(month %in% c(3:5), "spring",
                                ifelse(month %in% c(6:8),"summer",
                                       ifelse(month %in% c(9:11),"autumn",NA))))) %>%
  dplyr::select(c(rid,forest_types2020_iland,forest_type_eng,year,month,season,buff_maxT,buff_minT,buff_meanT))

# output
write.csv(buff.summary, paste0(path,"/monthly_buffering_",micro_year,"_1ha.csv"), row.names=FALSE)
