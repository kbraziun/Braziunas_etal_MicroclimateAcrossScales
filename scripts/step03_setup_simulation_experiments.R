#####
#
## Prep iLand simulation experiments
#
#####

### load libraries

library(tidyverse)
library(RSQLite)
library(terra)

### selected sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] terra_1.7-55    RSQLite_2.3.3   lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1  
# [6] dplyr_1.1.4     purrr_1.0.2     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1   
# [11] ggplot2_3.4.4   tidyverse_2.0.0

###
# 1. Identify climate years for summarizing buffering capacity
###

### choose 3 years based on low, average, and high mean annual temperature. Export average daily temperature buffering capacity for these three years from iLand for use in summarizing buffering across the landscape, by forest type, by season under historical climate

# load iland climate inputs
conn <-  DBI::dbConnect(RSQLite::SQLite(),
                        dbname = paste0("iland/database/historic_climate.sqlite")) # connect to the db
dbListTables(conn)

clim.out <- data.frame()

for(i in 1:length(dbListTables(conn))) {
  print(dbListTables(conn)[i])
  
  table.in <- dbListTables(conn)[i]
  
  # read and summarize annual climate
  clim.in <- tbl(conn, table.in) %>%
    mutate(table_name = table.in) %>%
    mutate(mean_temp = (min_temp + max_temp)/2,
           days = 1) %>%
    group_by(year,month,table_name) %>%
    summarise(across(c(min_temp,max_temp,mean_temp),mean), days=sum(days)) %>%
    collect()
  
  clim.out <- rbind(clim.out,clim.in)

  }

dbDisconnect(conn) # close the connection

write.csv(clim.out, "processed_data/iland_simulation_prep/climate_table_monthly.csv",row.names=FALSE)

# summarize by year, but calc daily averages (weighted average across months)
clim.yr <- clim.out %>%
  group_by(year) %>%
  summarise(across(c(min_temp,max_temp,mean_temp), ~weighted.mean(.,days)))

clim.yr %>%
  slice_max(mean_temp) # 1994

clim.yr %>%
  slice_min(mean_temp) # 1980

clim.yr %>%
  arrange(mean_temp) %>%
  slice(15) # 1988
# note this value is 5.7 C when restricted to only forested area

###
# 2. Set up main simulation experiment
###

### Simulation design
# Q: How does accounting for microclimate temperature buffering affect forest processes and change from stand to landscape scales?
# Approach: Simulate 1000 years of forest development in Berchtesgaden National Park, starting from 2020 forest conditions, in which processes are driven by either macroclimate or microclimate temperature (n = 10 reps of each x 2 climate drivers = 20 total reps). 

### Climate: Historical climate, resampling with replacement from historical record (1980-2009), fixed sequence of years for each replicate. 
# Choose same random order for each of 10 replicates

# number of years
2009-1980+1 # 30
# random seed
# sample(1:1000,10,replace=TRUE)
seeds <- c(178, 74, 745, 131, 342, 94, 930, 359, 340, 137)
# select 1000 years from 1:30
randSampList <- data.frame()
for(i in c(1:10)){
  print(i)

  set.seed(seeds[i])
  list <- sample(0:29,1000,replace=TRUE)
  randSampList <- rbind(randSampList,data.frame(rep=i, years=paste(list,collapse=",")))
  
}

write.table(randSampList, "iland/main_file.txt", row.names=FALSE)

### Wind: Historical wind speed and direction generated for 80 years with 20 replicates (1600 to select from), resampled with replacement, fixed sequence for each replicate.
# read in
list.files("data/iland_inputs/timeevents/")

wind.in <- list.files("data/iland_inputs/timeevents/", full.names=TRUE) %>%
  map(read_delim, delim=" ") %>%
  reduce(rbind) %>%
  dplyr::select(c(modules.wind.dayOfYear,modules.wind.speed,modules.wind.direction)) %>%
  mutate(samp = row_number())

# random seed
# paste(sample(1:1000,10,replace=TRUE),collapse=", ")
seeds <- c(555, 32, 153, 22, 181, 217, 199, 837, 478, 377)
# select 1000 years from 1:30
for(i in c(1:10)){
  print(i)
  
  set.seed(seeds[i])
  list <- sample(1:nrow(wind.in),1000,replace=TRUE)
  windList <- data.frame(samp = list) %>%
    left_join(wind.in, by="samp") %>%
    mutate(year=row_number()) %>%
    dplyr::select(c(year,modules.wind.dayOfYear,modules.wind.speed,modules.wind.direction))
  
  write.table(windList, paste0("iland/scripts/timeevents_",i,".txt"),quote=FALSE,sep=" ",row.names=FALSE)
}

# quick comparison
wind.comp <- list.files("data/iland_inputs/timeevents/", full.names=TRUE) %>%
  map_df(read_delim, delim=" ", .id="group") %>%
  mutate(group = paste0("orig_",group)) %>%
  dplyr::select(c(group,year,modules.wind.dayOfYear,modules.wind.speed,modules.wind.direction))

wind.new <- list.files("iland/scripts/", full.names=TRUE, pattern="timeevents_[1-9]") %>%
  map_df(read_delim, delim=" ", .id="group") %>%
  mutate(group = paste0("new_",group)) %>%
  dplyr::select(c(group,year,modules.wind.dayOfYear,modules.wind.speed,modules.wind.direction))

wind.comp %>%
  rbind(wind.new) %>%
  dplyr::select(-year) %>%
  pivot_longer(-group) %>%
  ggplot(aes(x=group,y=value,group=group)) +
  facet_wrap(~name, scales="free", nrow=3) +
  geom_boxplot() +
  theme_bw()

###
# 3. prep data for output processing/matchup
###

### load iland forest grids
standgrid.in <- rast("iland/gis/standgrid.asc")
# set -2 to NA
standgrid.in[standgrid.in < 0] <- NA
crs(standgrid.in) <- "epsg:31468"

# create unique ids for each 10m cell in standgrid
# classify everything as 1
grid.noseed <- classify(standgrid.in, matrix(c(-Inf,0,NA,1,Inf,1),ncol=3,byrow=TRUE))

# create copy of raster with unique ids, mask to same grid
id.rast <- rast(crs=crs(grid.noseed), extent=ext(grid.noseed), resolution=res(grid.noseed), vals=c(1:ncell(grid.noseed))) * grid.noseed

writeRaster(id.rast, "processed_data/iland_simulation_prep/stand_id_raster.tif", overwrite=TRUE)
