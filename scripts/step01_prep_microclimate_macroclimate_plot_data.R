#####
#
## Prep micro- and macroclimate data, plot-level predictors
#
#####

### load libraries
library(tidyverse)
library(openxlsx)
library(lubridate) # ymd, yday
library(zoo) # rollapply, rollmean, yearmon
library(sf)
library(terra)
library(RSQLite)

### selected sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] RSQLite_2.3.3    terra_1.7-55     sf_1.0-14        zoo_1.8-12       openxlsx_4.2.5.2
# [6] lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4      purrr_1.0.2     
# [11] readr_2.1.4      tidyr_1.3.0      tibble_3.2.1     ggplot2_3.4.4    tidyverse_2.0.0 

###
# 1. prep Zellweger et al. 2019
###

### plot coordinates
# include already calc'd data: elevation (not predictor), coordinates, tpi, and northness; retain distcoast for later consideration
zell.predin <- read.csv("data/microclimate_data/Zellweger_etal2019/Zellweger_etal2019_data.csv", skip=32) %>%
  dplyr::select(c(id,Easting,Northing,North,RelEmin500,alt,distcoast)) %>%
  # only 1 entry per plot
  distinct() %>%
  # consistent naming
  rename(plot_id = id,
         lon = Easting, lat = Northing,
         northness=North,tpi=RelEmin500,elev_m=alt) %>%
  mutate(data_source = "zellweger")
head(zell.predin)
summary(zell.predin)

# ck only 1 per plot
zell.predin %>%
  group_by(plot_id) %>%
  tally() %>%
  summary()

### microclimate and macroclimate temperature
zell.in <- read.table("data/microclimate_data/Zellweger_etal2019/microclimate/Alldailydat_allregions_22022017_21022018_def.txt", sep=",", header=TRUE) %>%
  # date format
  mutate(date = as.Date(date)) 
head(zell.in)
summary(zell.in)

zell.temp <- zell.in %>%
  # min, max, and mean daily microclimate temperature
  dplyr::select(c(id,date, min,max,mean, bufmin_c, bufmax_c, bufmean_c)) %>%
  # consistent naming
  rename(plot_id = id,
         micro_minT = min, micro_maxT = max, micro_meanT = mean, buff_minT = bufmin_c,
         buff_maxT = bufmax_c, buff_meanT = bufmean_c) %>%
  # add macroclimate temperature
  mutate(macro_minT = micro_minT - buff_minT,
         macro_maxT = micro_maxT - buff_maxT,
         macro_meanT = micro_meanT - buff_meanT) %>%
  mutate(data_source = "zellweger")

### forest inventory
# read in tree data
zell.treesin <- read.csv("data/microclimate_data/Zellweger_etal2019/treedat/DBHdata.csv", fileEncoding="UTF-8-BOM") %>%
  dplyr::select(c(Plot.ID,Species,DBH..cm.))

zell.lookup <- read.csv("processed_data/microclimate_data_prep/tree_species_lookup.csv") %>%
  dplyr::select(-comments)

zell.trees <- zell.treesin %>%
  left_join(zell.lookup, by = c("Species" = "zellSpecies")) %>%
  # drop 2 entries with no DBH
  filter(!is.na(DBH..cm.)) %>%
  rename(plot_id = Plot.ID,
         species_short = shortName_iLand,
         dbh_cm = DBH..cm.) %>%
  dplyr::select(plot_id,species_short,dbh_cm,alt_stol,alt_isConiferous,alt_isEvergreen) %>%
  # add plot size, circular with 9 m radius
  mutate(plotsize_m2 = (pi*9^2)) %>%
  mutate(data_source = "zellweger")

###
# 2. prep FORMICA
###

### plot coordinates
formica.coordsin <- read.xlsx("data/microclimate_data/FORMICA/Coordinates.xlsx", sheet="Sheet1") %>%
  # fix some plot names so match sensors
  mutate(plotID = ifelse(plotID=="CST1","CSLOT1",
                         ifelse(plotID=="CST2","CSLOT2",
                                ifelse(plotID=="CST3","CSLOT3",
                                       ifelse(plotID=="SST1","SSLOT1",
                                              ifelse(plotID=="SST2","SSLOT2",
                                                     ifelse(plotID=="SST3","SSLOT3",plotID))))))) %>%
  # unique plot is plotID plus location
  mutate(plot_id = toupper(paste0(plotID,"P",location))) %>%
  rename(lat = LAT,lon=LON, elev_m=HMSL) %>%
  mutate(across(c(lat,lon,elev_m),~as.numeric(.))) %>%
  dplyr::select(c(plot_id,lon,lat, elev_m)) 

# 1 sensor missing coordinates, assign it same value as nearest plot (3 m away)
formica.coords <- formica.coordsin %>%
  mutate(lon = ifelse(plot_id=="BELOT2P1",formica.coordsin[formica.coordsin$plot_id=="BELOT2P2",]$lon,lon),
         lat = ifelse(plot_id=="BELOT2P1",formica.coordsin[formica.coordsin$plot_id=="BELOT2P2",]$lat,lat),
         elev_m = ifelse(plot_id=="BELOT2P1",formica.coordsin[formica.coordsin$plot_id=="BELOT2P2",]$elev_m,elev_m)) %>%
  mutate(data_source = "formica")

### microclimate temperature
# read in data
formica.in <- read.xlsx("data/microclimate_data/FORMICA/all_Air.xlsx", sheet="data") %>%
  rename(date=Date) %>%
  mutate(date = ymd(convertToDate(date)))

# check for complete dates
formica.in %>%
  dplyr::select(c("date",ends_with("check"))) %>%
  group_by(date) %>%
  # if add up to 24, full day has good data
  summarise(across(everything(), ~sum(.x, na.rm=TRUE))) %>%
  dim() # 1667 days in dataset

length(seq(as.Date(min(formica.in$date)),as.Date(max(formica.in$date)),by = 1)) # 1667

# check=1 for good data
formica.cks <- formica.in %>%
  dplyr::select(c("date",ends_with("check"))) %>%
  group_by(date) %>%
  # if add up to 24, full day has good data
  summarise(across(everything(), ~sum(.x, na.rm=TRUE))) %>%
  pivot_longer(-date) %>%
  separate(name, into=c("plot_id","check"))

# min, max, and mean daily microclimate temp
formica.micro <- formica.in %>%
  dplyr::select(c("date",!ends_with("check"))) %>%
  group_by(date) %>%
  # if NAs in there, does not have 24 hrs and do not want to use value, so keep as NA
  summarise(across(everything(), list(minT=min,meanT=mean,maxT=max), .names = "{.col}_{.fn}")) %>%
  pivot_longer(-date) %>%
  separate(name, into=c("plot_id","temp")) %>%
  pivot_wider(names_from = temp, names_prefix = "micro_") %>%
  # add check
  left_join(formica.cks, by=c("date","plot_id")) %>%
  # set temp to NA if check < 24
  mutate(across(c(micro_minT,micro_meanT,micro_maxT), ~ifelse(value==24,.,NA))) %>%
  # drop "R" plots, not part of microclimate dataset
  filter(! plot_id %in% c("BEHIT1R","BEMET1R"))
  
formica.micro %>%
  filter(value<24) %>%
  summary() # good

formica.micro %>%
  filter(value==24) %>%
  summary() # good

### macroclimate temperature
formica.macroin <-  read.xlsx("data/microclimate_data/FORMICA/all_Regional.xlsx", sheet="data") %>%
  rename(date=Date) %>%
  mutate(date = ymd(convertToDate(date)))

# check for complete dates
formica.macroin %>%
  dplyr::select(c("date",ends_with("check"))) %>%
  group_by(date) %>%
  # if add up to 24, full day has good data
  summarise(across(everything(), ~sum(.x, na.rm=TRUE))) %>%
  dim() # 1667 days in dataset

length(seq(as.Date(min(formica.macroin$date)),as.Date(max(formica.macroin$date)),by = 1)) # 1667

# check=1 for good data
formica.macrocks <- formica.macroin %>%
  dplyr::select(c("date",ends_with("check"))) %>%
  group_by(date) %>%
  # if add up to 24, full day has good data
  summarise(across(everything(), ~sum(.x, na.rm=TRUE))) %>%
  pivot_longer(-date) %>%
  separate(name, into=c("plot_id","check"))

# min, max, and mean daily macroclimate temp
formica.macro <- formica.macroin %>%
  dplyr::select(c("date",!ends_with("check"))) %>%
  group_by(date) %>%
  # if NAs in there, does not have 24 hrs and do not want to use value, so keep as NA
  summarise(across(everything(), list(minT=min,meanT=mean,maxT=max), .names = "{.col}_{.fn}")) %>%
  pivot_longer(-date) %>%
  separate(name, into=c("plot_id","temp")) %>%
  pivot_wider(names_from = temp, names_prefix = "macro_") %>%
  # keep only air plots
  filter(grepl('A',plot_id)) %>%
  # add check
  left_join(formica.macrocks, by=c("date","plot_id")) %>%
  # set temp to NA if check < 24
  mutate(across(c(macro_minT,macro_meanT,macro_maxT), ~ifelse(value==24,.,NA))) %>%
  # rename plot id
  rename(macro_plot_id=plot_id)

formica.macro %>%
  filter(value<24) %>%
  summary() # good

formica.macro %>%
  filter(value==24) %>%
  summary() # good

### match micro and macro temps
# write.csv(data.frame(plot_id = unique(formica.micro$plot_id)),"processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_macro_lookup_formica.csv", row.names = FALSE)

formica.tempmatch <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_macro_lookup_formica.csv") 

formica.temp <- formica.micro %>%
  # join with macro data
  dplyr::select(c(date,plot_id,micro_minT,micro_meanT,micro_maxT)) %>%
  left_join(formica.tempmatch, by="plot_id") %>%
  left_join(formica.macro, by=c("date","macro_plot_id")) %>%
  # calc buffer
  mutate(buff_minT = micro_minT - macro_minT,
         buff_meanT = micro_meanT - macro_meanT,
         buff_maxT = micro_maxT - macro_maxT,
         data_source = "formica") %>%
  # clean up
  dplyr::select(c(date:micro_maxT,buff_minT,buff_meanT,buff_maxT,macro_minT,macro_meanT,macro_maxT,data_source)) 

summary(formica.temp)

### forest inventory
# read in tree data
formica.treesin <- read.xlsx("data/microclimate_data/FORMICA/Data_Composition_Circumference_finaal.xlsx", sheet="Composition") %>%
  mutate(plot_id = paste0(Region,Elevation,Type,Plot)) 

formica.lookup <- read.csv("processed_data/microclimate_data_prep/tree_species_lookup.csv") %>%
  dplyr::select(-comments)

formica.treesin %>%
  # filter(is.na(Species)) # 1 NA, assign to representative species in this plot
  filter(plot_id=="SWLOT3P3") # Ace.pla and Fag.syl are most common, Ace.pla more similar in size to NA species

formica.trees <- formica.treesin %>%
  # exclude stumps and dead trees, keep multi-stem and treat as separate trees
  filter(is.na(Stump) | Stump !="T") %>%
  filter(is.na(Dead) | Dead !="T") %>%
  # average dbh measurements (DBH1 and DBH2 are 2 sep measurements)
  mutate(dbh_cm = (DBH1+DBH2)/2) %>%
  # assign species to NA value
  mutate(Species = ifelse(is.na(Species),"Ace.pla",Species)) %>%
  # match up with lookup
  left_join(formica.lookup, by=c("Species"="formicaSpecies")) %>%
  rename(species_short=shortName_iLand) %>%
  # add plot size, circular plot with 9 m radius
  mutate(plotsize_m2 = (pi*9^2)) %>%
  mutate(data_source = "formica") %>%
  # clean up
  dplyr::select(c(plot_id,species_short,dbh_cm,alt_stol,alt_isConiferous,alt_isEvergreen,plotsize_m2,data_source))

###
# 3. prep Diaz-Calafat
###

### plot coordinates
dc.coords <- read.xlsx("data/microclimate_data/DiazCalafat_etal2023/Microclimatedata_CLIMIX_1yr.xlsx", sheet="PLOT COORDINATES") %>%
  # replace umlaut
  mutate(Site = ifelse(Site=="Sandsjö","Sandsjo",Site)) %>%
  # rename, merge into 1 plotID
  rename(lon=X, lat=Y) %>%
  mutate(plot_id = paste0(Site,"_",Plot),
         across(c(lon,lat),~as.numeric(.))) %>%
  # placeholder for elevation, extract later from DEM
  mutate(elev_m = NA,
         data_source="diazCalafat") %>%
  # clean up
  dplyr::select(c(plot_id,lon,lat,elev_m,data_source))
  
### microclimate and macroclimate data
dc.in <- read.xlsx("data/microclimate_data/DiazCalafat_etal2023/Microclimatedata_CLIMIX_1yr.xlsx",sheet="DATA") %>%
  rename(date=Date) %>%
  mutate(date = ymd(convertToDate(date)),
         plot_id = paste0(Site,"_",Plot)) %>%
  # convert values to numeric
  mutate(across(c(Logger_mean,Logger_min,Logger_max,mcera5_max,mcera5_min,mcera5_mean),~as.numeric(.)))

# macroclimate is mcera5 here, note that wx stations perform better per the manuscript

### read in wx stn macroclimate data
arville <- read.csv("data/microclimate_data/DiazCalafat_etal2023/ForestMicroclimate_Brodleaves_and_Density-main/WEATHER STATIONS/Arville_station6476.csv") %>%
  # only keep station 6476
  filter(code==6476) %>%
  dplyr::select(c(code,timestamp,temp)) %>%
  # add count to ensure 24 hrs
  mutate(count=ifelse(!is.na(temp),1,0)) %>%
  mutate(date=as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(macro_minT = min(temp, na.rm=TRUE),
            macro_maxT = max(temp, na.rm=TRUE),
            count=sum(count)) %>%
  mutate(macro_meanT = (macro_minT + macro_maxT)/2) %>%
  mutate(Site="Arville") %>%
  # na if less than 22 hours in a day
  mutate(across(c(macro_minT,macro_maxT,macro_meanT), ~ifelse(count<22,NA,.))) %>%
  dplyr::select(-count)

hochwald <- read.table("data/microclimate_data/DiazCalafat_etal2023/ForestMicroclimate_Brodleaves_and_Density-main/WEATHER STATIONS/Hochwald_station_airtemp.txt", header=TRUE, sep=";") %>%
  rename(temp=TT_TU) %>%
  mutate(date=as.Date(substr(MESS_DATUM,1,8), format = "%Y%m%d")) %>%
  # add nas for na_flag
  mutate(temp = ifelse(temp==-999, NA,temp)) %>%
  # add count to ensure 24 hrs
  mutate(count=ifelse(!is.na(temp),1,0)) %>%
  group_by(date) %>%
  summarise(macro_minT = min(temp, na.rm=TRUE),
            macro_maxT = max(temp, na.rm=TRUE),
            count=sum(count)) %>%
  # set anything inf to na
  mutate(macro_minT = ifelse(macro_minT %in% c("Inf","-Inf"),NA,macro_minT),
         macro_maxT = ifelse(macro_maxT %in% c("Inf","-Inf"),NA,macro_maxT)) %>%
  mutate(macro_meanT = (macro_minT + macro_maxT)/2) %>%
  mutate(Site="Hochwald") %>%
  # na if less than 22 hours in a day
  mutate(across(c(macro_minT,macro_maxT,macro_meanT), ~ifelse(count<22,NA,.))) %>%
  dplyr::select(-count)

sandsjo <- read.table("data/microclimate_data/DiazCalafat_etal2023/ForestMicroclimate_Brodleaves_and_Density-main/WEATHER STATIONS/Sandsjo_station.txt", header=TRUE, sep=";") %>%
  mutate(date=as.Date(Datum)) %>%
  rename(macro_minT = air_mintemp,
         macro_maxT = air_maxtemp,
         macro_meanT = air_meantemp) %>%
  mutate(Site="Sandsjo") %>%
  dplyr::select(c(date,macro_minT,macro_maxT,macro_meanT,Site))

siljan <- read.table("data/microclimate_data/DiazCalafat_etal2023/ForestMicroclimate_Brodleaves_and_Density-main/WEATHER STATIONS/Siljan_station.txt", header=TRUE, sep=";") %>%
  mutate(date=as.Date(Datum)) %>%
  rename(macro_minT = air_mintemp,
         macro_maxT = air_maxtemp,
         macro_meanT = air_meantemp) %>%
  mutate(Site="Siljan") %>%
  dplyr::select(c(date,macro_minT,macro_maxT,macro_meanT,Site))

vivarp <- read.csv("data/microclimate_data/DiazCalafat_etal2023/ForestMicroclimate_Brodleaves_and_Density-main/WEATHER STATIONS/Vivarp_station.csv", sep=";") %>%
  mutate(date = as.Date(Datum)) %>%
  rename(temp=Lufttemperatur) %>%
  # add count to ensure 24 hrs
  mutate(count=ifelse(!is.na(temp),1,0)) %>%
  group_by(date) %>%
  summarise(macro_minT = min(temp, na.rm=TRUE),
            macro_maxT = max(temp, na.rm=TRUE),
            count=sum(count)) %>%
  mutate(macro_meanT = (macro_minT + macro_maxT)/2) %>%
  mutate(Site="Vivarp") %>%
  # na if less than 22 hours in a day
  mutate(across(c(macro_minT,macro_maxT,macro_meanT), ~ifelse(count<22,NA,.))) %>%
  dplyr::select(-count)

# combine wx stns
dc.macro <- rbind(arville,hochwald,sandsjo,siljan,vivarp)

# calc buffering capacity and rename, switch in wx stn macro for mcera
dc.temp <- dc.in %>%
  left_join(dc.macro, by=c("date","Site")) %>%
  rename(micro_minT=Logger_min, micro_meanT=Logger_mean, micro_maxT=Logger_max) %>%
  # calc buffer
  mutate(buff_minT = micro_minT-macro_minT,
         buff_meanT = micro_meanT-macro_meanT,
         buff_maxT = micro_maxT-macro_maxT) %>%
  # data source
  mutate(data_source="diazCalafat") %>%
  # clean up
  dplyr::select(c(date,plot_id,micro_minT,micro_meanT,micro_maxT,buff_minT,buff_meanT,buff_maxT,macro_minT,macro_meanT,macro_maxT,data_source))

### forest inventory
# read in tree data
dc.treesin <- read.xlsx("data/microclimate_data/DiazCalafat_etal2023/Microclimatedata_CLIMIX_1yr.xlsx", sheet="TREE DATA") %>%
  mutate(plot_id = paste0(Site,"_",Plot)) 

dc.lookup <- read.csv("processed_data/microclimate_data_prep/tree_species_lookup.csv") %>%
  dplyr::select(-comments)

# additional prep
dc.treeprep <- dc.treesin %>%
  # exclude 1 tree missing DBH
  filter(!is.na(DBH)) %>%
  # dbh from mm to cm
  mutate(dbh_cm = DBH/10) %>%
  # match up with lookup
  left_join(dc.lookup, by=c("Species"="dcSpecies")) %>%
  rename(species_short=shortName_iLand) %>%
  mutate(data_source = "diazCalafat")

dc.treeprep %>%
  filter(is.na(Distance))

# consistent with other datasets, only trees within 9 m and > 7.5 cm DBH
dc.trees <- dc.treeprep %>%
  # subset to only trees within 9 m radius, if distance is NA still include since most likely in plot
  filter(Distance <=9 | is.na(Distance)) %>%
  # subset to trees > 7.5 cm DBH
  filter(dbh_cm >=7.5) %>%
  mutate(plotsize_m2 = (pi*9^2)) %>%
  # clean up
  dplyr::select(c(plot_id,species_short,dbh_cm,alt_stol,alt_isConiferous,alt_isEvergreen,plotsize_m2,data_source))


###
# 4. identify problematic microclimate data and outliers
###

### micro- and macroclimate temperature
temp.full <- zell.temp %>%
  rbind(formica.temp) %>%
  rbind(dc.temp)

# check for dates with missing data, fill in with NA, move this down and apply to full dataset
plotlist <- unique(temp.full$plot_id)
data.summ <- data.frame()
temp.out <- data.frame()

for(i in 1:length(plotlist)) {
  print(i)

  # subset to 1 plot
  plot.sel <- plotlist[i]

  temp.sub <- temp.full %>%
    filter(plot_id == plot.sel)
  
  # remove NA values for identifying first and last date with data
  temp.nas <- temp.sub %>%
    filter(!is.na(micro_minT))

  # create master version with all dates
  temp.ck <- data.frame(date = seq(as.Date(min(temp.nas$date)),as.Date(max(temp.nas$date)),by = 1),
                        plot_id = plot.sel,
                        data_source = unique(temp.sub$data_source)) %>%
    left_join(temp.sub, by=c("date","plot_id","data_source"))

  # summarize missing values
  data.summ <- rbind(data.summ,
                     data.frame(plot_id = plot.sel,
                          n_days = nrow(temp.ck),
                          n_missing_orig = nrow(temp.ck) - nrow(temp.sub), # ignore for formica plots
                          n_minT_na = nrow(temp.ck %>% filter(is.na(buff_minT))),
                          n_maxT_na = nrow(temp.ck %>% filter(is.na(buff_maxT)))))
  
  # output version with filled dates
  temp.out <- rbind(temp.out,temp.ck)

}

data.summ
write.csv(temp.out, "processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_macro_temp_raw.csv",row.names=FALSE)

### add snow cover flag
# code modified from https://github.com/poniitty/varrio_microclimate, ref Aalto et al. 2022
# used simplified criteria from Vilna Tyystjärvi et al. 2023 (preprint)
# when abs daily diff < 1 C, t range over 9-day moving window is < 2 C, max T < 1 C then = snow
# 2nd pass: assign all days within 5-day moving window to snow if at least 1 snow day
# also do this for macroclimate data

temp.snow <- data.frame()

for(i in 1:length(plotlist)) {
  print(i) 
  
  # subset to 1 plot
  plot.sel <- plotlist[i]
  
  temp.sub <- temp.out %>%
    filter(plot_id == plot.sel)
  
  # add snow flag
  snow.sub <- temp.sub %>%
    # absolute difference in daily temp
    mutate(abs_daily_diff = abs(micro_maxT-micro_minT),
           abs_daily_diff_macro = abs(macro_maxT - macro_minT)) %>%
    # max and min T in 9-day moving window
    mutate(roll_maxT = rollapply(micro_maxT, width=9, FUN=max, fill = NA, partial=TRUE, na.rm=TRUE),
           roll_maxT_macro = rollapply(macro_maxT, width=9, FUN=max, fill = NA, partial=TRUE, na.rm=TRUE)) %>%
    mutate(roll_minT = rollapply(micro_minT, width=9, FUN=min, fill = NA, partial=TRUE, na.rm=TRUE),
           roll_minT_macro = rollapply(macro_minT, width=9, FUN=min, fill = NA, partial=TRUE, na.rm=TRUE)) %>%
    # T range in 9-day moving window
    mutate(roll_rangeT = roll_maxT - roll_minT,
           roll_rangeT_macro = roll_maxT_macro - roll_minT_macro) %>%
    # snow flag if daily range < 1C, 9-day range <2C, maxT <1C
    mutate(snow1 = ifelse(abs_daily_diff <= 1 & roll_maxT <= 1 & roll_rangeT <= 2, 1,0),
           snow1_macro = ifelse(abs_daily_diff_macro <= 1 & roll_maxT_macro <= 1 & roll_rangeT_macro <= 2, 1,0)) %>%
    # also assign 0 to NA values
    mutate(snow1 = ifelse(is.na(snow1),0,snow1),
           snow1_macro = ifelse(is.na(snow1_macro),0,snow1_macro)) %>%
    # snow flag if any snow days in 5-day moving window
    mutate(snow = rollapply(snow1, width=5, FUN=max, fill=NA, partial=TRUE, na.rm=TRUE),
           snow_macro = rollapply(snow1_macro, width=5, FUN=max, fill=NA, partial=TRUE, na.rm=TRUE)) %>%
    # clean up
    dplyr::select(-c(abs_daily_diff:snow1_macro))
  
  # output version with snow flag
  temp.snow <- rbind(temp.snow,snow.sub)

}

# warnings: assigned Inf if all = NA, can ignore

# # examine regions with snow
# region <- "CSLOT"
# region <- "NOHIT"
# region <- "NOLOT"
# region <- "NOMET"
#  
# datemin <- "2018-01-01"
# datemax <- "2022-01-01"
# 
# temp.snow %>%
#   filter(grepl(region,plot_id),
#          date %in% seq(as.Date(datemin),as.Date(datemax),by = 1)) %>%
#   ggplot(aes(x=date,y=micro_maxT)) +
#   facet_wrap(~snow) +
#   geom_point(aes(color=factor(snow))) +
#   geom_smooth()
# 
# temp.snow %>%
#   filter(grepl(region,plot_id),
#          date %in% seq(as.Date(datemin),as.Date(datemax),by = 1)) %>%
#   ggplot(aes(x=date,y=micro_minT)) +
#   facet_wrap(~snow) +
#   geom_point(aes(color=factor(snow))) +
#   geom_smooth()
# 
# temp.snow %>%
#   filter(grepl(region,plot_id),
#          date %in% seq(as.Date(datemin),as.Date(datemax),by = 1)) %>%
#   ggplot(aes(x=date,y=macro_maxT)) +
#   facet_wrap(~snow_macro) +
#   geom_point(aes(color=factor(snow_macro))) +
#   geom_smooth()

### identify outliers, potentially problematic loggers

# read in region lookup
region.in <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_region_lookup.csv")

# based on regional data, flag outlier values
# roughly following guidelines from Aalto et al. 2022
# code modified from https://github.com/poniitty/varrio_microclimate
# use ~monthly moving average (30-day), simple stats (std dev), flag outlier values and use to identify potentially problematic loggers
temp.outregion <- temp.snow %>%
  left_join(region.in, by="plot_id") %>%
  group_by(region) %>%
  # rolling 30-day SD for each region
  mutate(sd_maxT = rollapply(micro_maxT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE),
         sd_minT = rollapply(micro_minT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE)) %>%
  ungroup() %>%
  # mean value for each date and region
  group_by(date,region) %>%
  mutate(mean_maxT = mean(micro_maxT,na.rm=TRUE),
         mean_minT = mean(micro_minT, na.rm=TRUE)) %>%
  ungroup() %>%
  # look for deviations by plot
  # count of non-NA vaues
  mutate(count_maxT = ifelse(!is.na(micro_maxT),1,0),
         count_minT = ifelse(!is.na(micro_minT),1,0)) %>%
  # identify values > 1, 2, or 3 SD from mean
  mutate(sd1_max = ifelse(abs(micro_maxT - mean_maxT) > abs(sd_maxT),1,0),
         sd2_max = ifelse(abs(micro_maxT - mean_maxT) > 2*abs(sd_maxT),1,0),
         sd3_max = ifelse(abs(micro_maxT - mean_maxT) > 3*abs(sd_maxT),1,0)) %>%
  mutate(sd1_min = ifelse(abs(micro_minT - mean_minT) > abs(sd_minT),1,0),
         sd2_min = ifelse(abs(micro_minT - mean_minT) > 2*abs(sd_minT),1,0),
         sd3_min = ifelse(abs(micro_minT - mean_minT) > 3*abs(sd_minT),1,0)) 

# inspect problematic loggers
region.sd3 <- temp.outregion %>%
  group_by(region) %>%
  # remove snow dates
  mutate(sd3_max = ifelse(snow==1,0,sd3_max),
         sd3_min = ifelse(snow==1,0,sd3_min)) %>%
  # loggers that strongly deviate from regional values
  summarise(sd_max_prop = sum(sd3_max,na.rm=TRUE)/sum(count_maxT),
            sd_min_prop = sum(sd3_min,na.rm=TRUE)/sum(count_minT)) %>%
  # only regions with some flags
  filter(sd_max_prop>0 | sd_min_prop>0)

plotregion.sd3 <- temp.outregion %>%
  group_by(plot_id,region) %>%
  # remove snow dates
  mutate(sd3_max = ifelse(snow==1,0,sd3_max),
         sd3_min = ifelse(snow==1,0,sd3_min)) %>%
  # loggers that strongly deviate from regional values
  summarise(sd_max_prop = sum(sd3_max,na.rm=TRUE)/sum(count_maxT),
            sd_min_prop = sum(sd3_min,na.rm=TRUE)/sum(count_minT)) %>%
  # only regions with some flags
  filter(sd_max_prop>0 | sd_min_prop>0) # 86 flagged

i <- 1
region.sub <- region.sd3[i,]$region
region.sub
plotregion.sd3 %>%
  filter(region==region.sub)

# can look finer detail within region
# region.sub <- "CSLOT1"

temp.outregion %>%
  filter(grepl(region.sub,plot_id)) %>%
  ggplot(aes(x=date,y=micro_maxT, color=factor(plot_id))) +
  geom_point() +
  geom_smooth()

temp.outregion %>%
  filter(grepl(region.sub,plot_id)) %>%
  ggplot(aes(x=date,y=buff_maxT, color=factor(plot_id))) +
  geom_point() +
  geom_smooth()

temp.outregion %>%
  filter(grepl(region.sub,plot_id)) %>%
  ggplot(aes(x=date,y=micro_minT, color=factor(plot_id))) +
  geom_point() +
  geom_smooth()

temp.outregion %>%
  filter(grepl(region.sub,plot_id)) %>%
  ggplot(aes(x=date,y=buff_minT, color=factor(plot_id))) +
  geom_point() +
  geom_smooth()

# look at individual plot
plot.sub <- "CSLOT1P4"
temp.outregion %>%
  filter(grepl(plot.sub,plot_id)) %>%
  # filter(date<"2021-03-01") %>%
  # filter(date>"2018-11-30") %>%
  ggplot(aes(x=date,y=micro_maxT, color=factor(sd3_max))) +
  geom_point() 

temp.outregion %>%
  filter(grepl(plot.sub,plot_id)) %>%
  # filter(date<"2021-01-01") %>%
  # filter(date>"2018-11-30") %>%
  ggplot(aes(x=date,y=micro_minT, color=factor(sd3_min))) +
  geom_point()

temp.outregion %>%
  filter(grepl(plot.sub,plot_id)) %>%
  filter(snow!=1) %>%
  filter(sd3_max==1 | sd3_min==1) %>%
  # filter(date>"2019-01-01") %>%
  summary()

### identified issues
# BEHIT1P2, consistent issues after 2021-03-01
# BEHIT2P3, seems ok but issues all occur prior to 2018-07-01, so exclude
# BEMET2P2, not flagged because all other loggers NA, but issues 2020-05-01 to 2021-04-08
# BEMET2P3, issues 2019-11-25 to 2021-05-03
# CSLOT1P4, issues 2018-06-30 to 2021-09-27
# CSLOT3P1, issues 2018-12-24 to 2019-07-31
# GELOT2P4, issues before 2018-11-06
# ITHIT3P3, issues 2019-01-22 to 2019-04-05
# ITLOT2P3 consistently high summer temps, >60 C in 2021, buffering patterns differ from rest of transect, exclude full dataset
# ITLOT3P4, weird max T trends, inconsisent buffering patterns compared to others in transect, contrast with expectations
# NFLOT1-3, weird trend affecting all sensors in 2019-2020 winter, seems like they all have ~same max and min temp above freezing, buffering inconsistent with time series; but other transects similar, decide to omit 2019-10-01 to 2021-01-01
# NFLOT3P3, issues but addressed by previous
# NOLOT2, problematic min T before 2018-04-02, omit these
# NOMET2P1, issues 2018-07-01 to 2019-01-24
# NOMET3P3, issues 2020-03-26 to 2020-05-23
# SSLOT2P3, issues 2018-07-13 to 2018-12-27

### other notes
# multiple plots with only a few outliers, leaving in for now
# some issues already caught by snow flag
# some transects differ from region but consistent within transect, leave in

### assign excluded date ranges
temp.exclude <- rbind(cbind("BEHIT1P2","2021-03-01","2022-05-01"),
                      cbind("BEHIT2P3","2017-01-01","2018-07-01"),
                      cbind("BEMET2P2","2020-05-01","2021-04-08"),
                      cbind("BEMET2P3","2019-11-01","2020-05-04"),
                      cbind("CSLOT1P4","2018-06-01","2021-09-30"),
                      cbind("CSLOT3P1","2018-12-01","2019-07-31"),
                      cbind("GELOT2P4","2017-01-01","2018-11-30"),
                      cbind("ITHIT3P3","2019-01-01","2019-04-30"),
                      cbind("ITLOT2P3","2017-01-01","2022-05-01"),
                      cbind("ITLOT3P4","2017-01-01","2022-05-01"),
                      cbind("NFLOT1P1","2019-10-01","2021-01-01"),
                      cbind("NFLOT1P2","2019-10-01","2021-01-01"),
                      cbind("NFLOT1P3","2019-10-01","2021-01-01"),
                      cbind("NFLOT1P4","2019-10-01","2021-01-01"),
                      cbind("NFLOT1P5","2019-10-01","2021-01-01"),
                      cbind("NFLOT2P1","2019-10-01","2021-01-01"),
                      cbind("NFLOT2P2","2019-10-01","2021-01-01"),
                      cbind("NFLOT2P3","2019-10-01","2021-01-01"),
                      cbind("NFLOT2P4","2019-10-01","2021-01-01"),
                      cbind("NFLOT2P5","2019-10-01","2021-01-01"),
                      cbind("NFLOT3P1","2019-10-01","2021-01-01"),
                      cbind("NFLOT3P2","2019-10-01","2021-01-01"),
                      cbind("NFLOT3P3","2019-10-01","2021-01-01"),
                      cbind("NFLOT3P4","2019-10-01","2021-01-01"),
                      cbind("NFLOT3P5","2019-10-01","2021-01-01"),
                      cbind("NOLOT1P1","2017-01-01","2018-04-30"),
                      cbind("NOLOT1P2","2017-01-01","2018-04-30"),
                      cbind("NOLOT1P3","2017-01-01","2018-04-30"),
                      cbind("NOLOT1P4","2017-01-01","2018-04-30"),
                      cbind("NOLOT1P5","2017-01-01","2018-04-30"),
                      cbind("NOLOT2P1","2017-01-01","2018-04-30"),
                      cbind("NOLOT2P2","2017-01-01","2018-04-30"),
                      cbind("NOLOT2P3","2017-01-01","2018-04-30"),
                      cbind("NOLOT2P4","2017-01-01","2018-04-30"),
                      cbind("NOLOT2P5","2017-01-01","2018-04-30"),
                      cbind("NOLOT3P1","2017-01-01","2018-04-30"),
                      cbind("NOLOT3P2","2017-01-01","2018-04-30"),
                      cbind("NOLOT3P3","2017-01-01","2018-04-30"),
                      cbind("NOLOT3P4","2017-01-01","2018-04-30"),
                      cbind("NOLOT3P5","2017-01-01","2018-04-30"),
                      cbind("NOMET2P1","2018-06-15","2019-01-31"),
                      cbind("NOMET3P3","2020-03-01","2020-05-31"),
                      cbind("SSLOT2P3","2018-07-01","2019-01-15")) %>%
  as.data.frame() %>%
  rename("plot_id"="V1",
         "date_start"="V2",
         "date_end"="V3") %>% 
  mutate(across(c("date_start","date_end"),~as.Date(.)))

### add flagged dates
temp.flagregion <- data.frame()

for(i in 1:length(plotlist)) {
  print(i) 
  
  # subset to 1 plot
  plot.sel <- plotlist[i]
  
  temp.sub <- temp.snow %>%
    filter(plot_id == plot.sel)
  
  if(!plot.sel %in% c(temp.exclude$plot_id)) {
    
    temp.sub$excludeFlag_region <- 0
    
    temp.flagregion <- rbind(temp.flagregion,temp.sub)
  } else if(plot.sel %in% c(temp.exclude$plot_id)) {
    
    temp.excludesub <- temp.exclude %>%
      filter(plot_id==plot.sel)
    
    temp.sub$excludeFlag_region <- ifelse(temp.sub$date >= temp.excludesub$date_start & temp.sub$date <= temp.excludesub$date_end, 1, 0)
    
    # output version with flag
    temp.flagregion <- rbind(temp.flagregion,temp.sub)
  }
  
}

### also look for plot-level outliers

# then at plot level, again 30-day rolling SD
# identify high daily temperature change relative to SD
# also identify areas with extreme temps (abs()>50 C)
# do this for both micro and macroclimate data
temp.outplot <- temp.flagregion %>%
  group_by(plot_id) %>%
  # rolling 30-day SD for each plot
  mutate(sd_maxT = rollapply(micro_maxT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE),
         sd_minT = rollapply(micro_minT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE),
         mean_maxT = rollapply(micro_maxT, width=30, FUN=mean, fill = NA, partial=TRUE, na.rm=TRUE),
         mean_minT = rollapply(micro_minT, width=30, FUN=mean, fill = NA, partial=TRUE, na.rm=TRUE)) %>%
  # also for macroclimate
  mutate(sd_maxT_macro = rollapply(macro_maxT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE),
         sd_minT_macro = rollapply(macro_minT, width=30, FUN=sd, fill = NA, partial=TRUE, na.rm=TRUE),
         mean_maxT_macro = rollapply(macro_maxT, width=30, FUN=mean, fill = NA, partial=TRUE, na.rm=TRUE),
         mean_minT_macro = rollapply(macro_minT, width=30, FUN=mean, fill = NA, partial=TRUE, na.rm=TRUE)) %>%
  # look for daily deviations
  # count of non-NA vaues
  mutate(count_maxT = ifelse(!is.na(micro_maxT),1,0),
         count_minT = ifelse(!is.na(micro_minT),1,0),
         # also flag extreme values 
         extreme_maxT = ifelse(abs(micro_maxT)>50,1,0),
         extreme_minT = ifelse(abs(micro_minT)>50,1,0)) %>%
  # also for macroclimate
  mutate(count_maxT_macro = ifelse(!is.na(macro_maxT),1,0),
         count_minT_macro = ifelse(!is.na(macro_minT),1,0),
         # also flag extreme values 
         extreme_maxT_macro = ifelse(abs(macro_maxT)>50,1,0),
         extreme_minT_macro = ifelse(abs(macro_minT)>50,1,0)) %>%
  mutate(sd3_max = ifelse(abs(micro_maxT - mean_maxT) > 3*abs(sd_maxT),1,0)) %>%
  mutate(sd3_min = ifelse(abs(micro_minT - mean_minT) > 3*abs(sd_minT),1,0)) %>%
  mutate(sd3_max_macro = ifelse(abs(macro_maxT - mean_maxT_macro) > 3*abs(sd_maxT_macro),1,0)) %>%
  mutate(sd3_min_macro = ifelse(abs(macro_minT - mean_minT_macro) > 3*abs(sd_minT_macro),1,0))

# look at individual plots
plot.sub <- "ITLOT2P2"
temp.outplot %>%
  filter(grepl(plot.sub,plot_id)) %>%
  # remove values flagged for snow or region, to avoid duplicating efforts
  filter(snow==0, excludeFlag_region==0) %>%
  ggplot(aes(x=date,y=micro_maxT, color=factor(sd3_max))) +
  geom_point() 

temp.outplot %>%
  filter(grepl(plot.sub,plot_id)) %>%
  # remove values flagged for snow or region, to avoid duplicating efforts
  filter(snow==0, excludeFlag_region==0) %>%
  ggplot(aes(x=date,y=micro_minT, color=factor(sd3_min))) +
  geom_point()

temp.outplot %>%
  filter(grepl(plot.sub,plot_id)) %>%
  filter(snow!=1) %>%
  filter(sd3_max==1 | sd3_min==1) %>%
  # filter(date>"2019-01-01") %>%
  summary()

# add flag, exclude later
temp.outflag <- temp.outplot %>%
  mutate(excludeFlag_plot = ifelse(extreme_maxT==1 | extreme_minT==1 | sd3_max==1 | sd3_min==1, 1,0)) %>%
  mutate(excludeFlag_macro = ifelse(extreme_maxT_macro==1 | extreme_minT_macro==1 | sd3_max_macro==1 | sd3_min_macro==1, 1,0)) %>%
  dplyr::select(c(date:excludeFlag_region,excludeFlag_plot,excludeFlag_macro))

# write out flagged version
write.csv(temp.outflag, "processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_macro_temp_flagged.csv",row.names=FALSE)

###
# 5. merge coordinate and forest inventory datasets
###

### plot coordinates
coords.out <- zell.predin %>%
  dplyr::select(c(plot_id,lon,lat,elev_m,data_source)) %>%
  rbind(formica.coords) %>%
  rbind(dc.coords)

write.csv(coords.out, "processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_coords_all.csv",row.names=FALSE)

### trees
trees.out <- zell.trees %>%
  rbind(formica.trees) %>%
  rbind(dc.trees) 

write.csv(trees.out, "processed_data/microclimate_data_prep/exploratory_incremental_steps/forest_inventory_all.csv",row.names=FALSE)

###
# 6. prepare predictors derived from DEM
###

### read in data if needed
coords.out <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_coords_all.csv",header=TRUE) 

### derive DEM predictors for each plot
# function to convert to radians
deg2rad <- function(deg) {(deg * pi) / (180)}

# loop through tiles
plots.demout <- data.frame() # blank df for output

for(j in list.files("data/predictor_variables/EU_DEM/", pattern="cropped.tiff")){
  eu.tile = j
  print(eu.tile)
  
  eu.crop <- rast(paste0("data/predictor_variables/EU_DEM/",eu.tile))
  # plot(eu.crop)
  
  # crop and mask overlapping coordinates and eu tile
  # set buffer to 600 m, because focal stats are calculated with 500 m radius
  # check to make sure includes full buffer
  coords.sub <- coords.out %>% 
    st_as_sf(coords=c("lon","lat"), crs=4326) %>%
    st_transform(crs=crs(eu.crop)) %>%
    # subset to eu tile
    st_crop(eu.crop) 
  
  # loop through plots in tile
  
  for(i in 1:nrow(coords.sub)) {
    print(i)
    
    coord.i <- coords.sub[i,]
    
    coord.buff <- coord.i %>%
      # add buffer, 600 m to ensure includes focal stats at plot center within window
      st_buffer(dist=600)
    
    eu.plot <- crop(eu.crop,coord.buff) %>%
      mask(coord.buff)
    
    # derive slope and aspect, 4nbr rule to align with iland
    eu.topo <- terrain(eu.plot, v=c("slope","aspect"),unit="degrees",neighbors=4)
    
    # cosine transformation of aspect
    northness <- cos(deg2rad(eu.topo[[2]]))
    
    # topo position
    # there is an error in the spatialEco tpi function for circular buffers, so calc in steps using terra package
    # refer to: https://gis.stackexchange.com/questions/358923/how-can-i-get-the-correct-mean-from-focal-with-a-circular-window
    # Set circular window of 500m radius 
    fw <- focalMat(eu.plot, 500, "circle") 
    fw[fw > 0] <- 1   # replacing weights by 1
    fw[fw == 0] <- NA   # replacing 0s with NA
    
    #using the Terra version of the focal function
    # testing both tpi (rel to mean) and relative elevation or cold air drainage (rel to min)
    focal_r_spat_rast_mean <- terra::focal(x = eu.plot, w = fw, fun = "mean",na.rm=TRUE)
    focal_r_spat_rast_min <- terra::focal(x = eu.plot, w = fw, fun = "min",na.rm=TRUE)
    
    # tpi is elevation of cell minus mean elevation in this 500m radius
    tpi <- eu.plot - focal_r_spat_rast_mean
    rel_elev <- eu.plot - focal_r_spat_rast_min
    
    # stack final topo rasters
    pred.dem <- c(eu.plot, eu.topo[[1]], northness, tpi, rel_elev)
    names(pred.dem) <- c("elev_m","slope_deg","northness","tpi","rel_elev")
    
    # extract values for plot points
    plots.dem <- coord.i %>%
      dplyr::select(-c(elev_m,data_source)) %>%
      cbind(extract(pred.dem, coord.i)) %>%
      as.data.frame() %>%
      dplyr::select(-c(geometry,ID)) %>%
      mutate(tile_name = eu.tile)
    
    # combine into full dataset
    plots.demout <- rbind(plots.demout,plots.dem)
    
  }
}

plots.demout %>%
  dplyr::select(c(plot_id,elev_m,slope_deg,northness,tpi,rel_elev,tile_name)) %>%
  write.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/plots_dem_predictors.csv",row.names=FALSE)

# compare with predictors in zellweger dataset, make sure dem-derived values are consistent
zell.predin %>%
  left_join(plots.demout, by="plot_id") %>%
  ggplot(aes(x=elev_m.x, y=elev_m.y)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=0,slope=1,color="red")

zell.predin %>%
  left_join(plots.demout, by="plot_id") %>%
  ggplot(aes(x=northness.x, y=northness.y)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=0,slope=1,color="red")

zell.predin %>%
  left_join(plots.demout, by="plot_id") %>%
  ggplot(aes(x=tpi.x, y=rel_elev)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=0,slope=1,color="red")

###
# 7. additional predictors: distance to coast, phenology, forest structure and composition
###

### read climate data back in
temp.outflag <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_macro_temp_flagged.csv",header=TRUE) %>%
  mutate(date = as.Date(date)) %>%
  # include macro exclusions for formica loggers only
  mutate(snow = ifelse(data_source=="formica" & snow_macro==1,1,snow),
         excludeFlag_plot = ifelse(data_source=="formica" & excludeFlag_macro==1,1,excludeFlag_plot))

# all flags out, only for microclimate
temp.microflags <- temp.outflag %>% 
  dplyr::select(c(date,plot_id,data_source,micro_minT,micro_maxT,micro_meanT,snow,excludeFlag_region,excludeFlag_plot))

# set macroclimate designation
name <- "macroOriginal"
temp.select <- temp.outflag

### read in other data if needed
coords.out <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_coords_all.csv",header=TRUE) 
trees.out <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/forest_inventory_all.csv",header=TRUE)
plots.demout <- read.csv("processed_data/microclimate_data_prep/exploratory_incremental_steps/plots_dem_predictors.csv",header=TRUE)

### distance to coast
coast.in <- st_read("data/predictor_variables/Natural_Earth/ne_110m_coastline/ne_110m_coastline.shp")

# transform coords and coast to LAEA, distance will be in m
coords.laea <- coords.out %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326) %>%
  st_transform(crs=3035)

coast.laea <- coast.in[94,] %>% 
  st_transform(crs=crs(coords.laea))

# add distance to coast
coords.laea$distcoast_m <- st_distance(coords.laea, coast.laea, which="Euclidean") 

# output
distcoast <- coords.laea %>%
  mutate(distcoast_km = as.numeric(distcoast_m)/1000) %>%
  as.data.frame() %>%
  dplyr::select(plot_id,distcoast_km) 

### phenology: Growing Season Index
# use plot coordinates and day of year
# based on eq from WR, http://herbert.gandraxa.com/length_of_day.aspx
deg2rad <- function(deg) {(deg * pi) / (180)}

coords.rad <- coords.out %>%
  mutate(latitude_rad = deg2rad(lat)) %>%
  dplyr::select(c(plot_id,lat,latitude_rad))

# first daylength
# variables
day.j <- pi/182.625
day.ecliptic <- deg2rad(23.439)
LPmin <- 10 # 10 hrs
LPmax <- 11 # 11 hrs

temp.day <- temp.select %>%
  # add coordinates
  left_join(coords.rad, by="plot_id") %>%
  mutate(date = as.Date(date),
         # subtract 1 to align with 0 indexing in C++
         day = yday(date)-1) %>%
  # calc m; day=0: winter solstice => subtract 10 days
  mutate(m = 1 - tan(latitude_rad)*tan(day.ecliptic*cos(day.j*(day+10))),
         m = ifelse(m<0,0, ifelse(m>2,2,m))) %>%
  # result in hours [0..24]
  mutate(dayLength = acos(1-m)/pi * 24) %>%
  # calculate photoperiod from 0 to 1
  # https://iland-model.org/phenology, Jolly et al. 2005 (https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2486.2005.00930.x)
  mutate(fLP = ifelse(dayLength<=LPmin,0,
                      ifelse(dayLength>=LPmax,1,
                             (dayLength - LPmin)/(LPmax-LPmin))))
  
# next temperature
TPmin <- -2
TPmax <- 5

temp.tp <- temp.day %>%
  # calculate temp response from 0 to 1
  # https://iland-model.org/phenology, Jolly et al. 2005 (https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2486.2005.00930.x)
  mutate(fTP = ifelse(macro_minT<=TPmin,0,
                      ifelse(macro_minT>=TPmax,1,
                             (macro_minT - TPmin)/(TPmax-TPmin))))

# exclude VPD because macroclimate input data only includes temperature, VPD estimation directly from temperature is not well suited for hot/dry environments, temp and light generally dictate phenology limitations in iland

temp.day %>%
  filter(plot_id=="BI2388") %>%
  ggplot(aes(x=day)) +
  geom_ribbon(aes(ymin=fLP,ymax=1), fill="gray20")

temp.tp %>%
  filter(plot_id=="BI2388") %>%
  ggplot(aes(x=as.Date(date))) +
  geom_ribbon(aes(ymin=fTP,ymax=1), fill="blue")

# need 21-day rolling average
# double check that there are complete cases
temp.day %>%
  mutate(dateRun = as.numeric(date),
         counter=1) %>%
  group_by(plot_id) %>%
  summarise(n=sum(counter),
            days = max(dateRun)-min(dateRun)+1) %>%
  mutate(diff=n-days) %>%
  filter(diff!=0) # good
  
temp.phen <- temp.tp %>%
  dplyr::select(c(plot_id,date,fLP,fTP)) %>%
  arrange(plot_id,date) %>%
  # rolling 21-day mean, NA if missing values
  group_by(plot_id) %>%
  mutate(fLP_mean = rollmean(fLP,k=21, fill=NA,align="center"),
         fTP_mean = rollmean(fTP,k=21, fill=NA,align="center")) %>%
  mutate(GSI = fLP_mean * fTP_mean) %>%
  dplyr::select(c(plot_id,date,fLP_mean:GSI)) %>%
  mutate(year=year(date),
         month=month(date),
         day=day(date))

# check plots
plotlist <- unique(temp.phen$plot_id)

temp.phen %>%
  filter(plot_id %in% plotlist[c(1:25)]) %>%
  ggplot(aes(x=date)) +
  facet_wrap(~plot_id) +
  geom_ribbon(aes(ymin=fLP_mean,ymax=1), fill="gray20") +
  geom_ribbon(aes(ymin=fTP_mean,ymax=1), fill="blue") +
  # geom_ribbon(aes(ymin=fDP_mean,ymax=1), fill="red") +
  theme_bw()

# iland uses phenology (growing season on/off), derived from GSI
# identify first date that GSI exceeded 0.5, start of growing season
phen.start <- temp.phen %>%
  filter(!is.na(GSI)) %>%
  # add counter
  group_by(plot_id,year) %>%
  mutate(counter = row_number()) %>%
  # add flag for GSI > 0.5
  mutate(gsi05_flag = ifelse(GSI>0.5,1,0)) %>%
  # subset to first day for each year
  filter(gsi05_flag==1) %>%
  group_by(plot_id,year) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  # omit start dates if it is the first record for that year
  filter(counter>1) %>%
  dplyr::select(c(plot_id,year,month,date,counter)) %>%
  rename(month_start = month,
         date_start = date) %>%
  mutate(day_start = day(date_start))

# identify first date that GSI goes back below 0.5 after longest day of the year, end of growing season
phen.end <- temp.phen %>%
  # after june
  filter(month>6) %>%
  filter(!is.na(GSI)) %>%
  # add counter
  group_by(plot_id,year) %>%
  mutate(counter = row_number()) %>%
  # add flag for GSI < 0.5
  mutate(gsi05_flag = ifelse(GSI<0.5,0,1)) %>%
  # subset to first day for each year
  filter(gsi05_flag==0) %>%
  group_by(plot_id,year) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  # omit dates if it is the first record for that year
  filter(counter>1) %>%
  dplyr::select(c(plot_id,year,month,date,counter)) %>%
  rename(month_end = month,
         date_end = date) %>%
  mutate(day_end = day(date_end))

# now calc monthly value, before growing season set to 0, after set to 1, shoulder months based on proportion of days before/after start/end of growing season
month.phen <- temp.phen %>%
  ungroup() %>%
  left_join(phen.start, by=c("plot_id","year")) %>%
  left_join(phen.end, by=c("plot_id","year")) %>%
  # add number of days in each month
  group_by(plot_id,year,month) %>%
  mutate(month_days = max(day)) %>%
  ungroup() %>%
  # if prior to start month, set GSI to 0
  mutate(phenology = ifelse(month<month_start,0,
                      ifelse(month>month_start & month<month_end,1,
                                           ifelse(month>month_end,0,
                                                  ifelse(month==month_start, 1-(day_start)/month_days,
                                                         ifelse(month==month_end, 1-(month_days-(day_end))/month_days,NA)))))) %>%
  # add fix for NA phenology when data does not extend to begin/end of grow season
  mutate(phenology = ifelse(is.na(month_end) & !is.na(GSI),
                            ifelse(month<month_start, 0,
                                   ifelse(month>month_start,1,
                                          ifelse(month==month_start, 1-(day_start)/month_days,phenology))),phenology)) %>%
  mutate(phenology = ifelse(is.na(month_start) & !is.na(GSI),
                            ifelse(month<month_end, 1,
                                   ifelse(month>month_end,0,
                                          ifelse(month==month_end, 1-(month_days-(day_end))/month_days,phenology))),phenology)) %>%
  mutate(phenology = ifelse(!is.na(GSI) & is.na(month_start) & is.na(month_end),
                            ifelse(GSI<0.5,0,
                                   ifelse(GSI>0.5,1,phenology)),phenology))

summary(month.phen)

# check for excluded observations
month.phen %>%
  filter(is.na(phenology) & !is.na(GSI)) %>%
  summary()

month.phen %>%
  filter(!is.na(phenology),!is.na(GSI)) %>%
  ggplot(aes(x=month,y=phenology, group=month)) +
  geom_boxplot() +
  theme_bw() # looks good

month.phen %>%
  group_by(year,month,plot_id) %>%
  mutate(min=min(phenology),max=max(phenology),mean=mean(phenology),diff=max-min) %>% 
  filter(diff>0) # good

### forest inventory
# LAI and conifer share
# read in species parameters and match up
conn <-
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = paste0("data/iland_inputs/species_param_europe.sqlite")) # connect to the db
dbListTables(conn)

iland.in <- tbl(conn, "species") %>%
  dplyr::select(c(shortName,specificLeafArea,bmFoliage_a,bmFoliage_b,isConiferous,isEvergreen,lightResponseClass)) %>%
  collect() %>%
  mutate(across(c(specificLeafArea:lightResponseClass),as.numeric)) 

dbDisconnect(conn) # close the connection

# also PNW parameters
conn2 <-
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = paste0("data/iland_inputs/species_param_PNW_selected.sqlite")) # connect to the db
dbListTables(conn2)

iland.in2 <- tbl(conn2, "species") %>%
  dplyr::select(c(shortName,specificLeafArea,bmFoliage_a,bmFoliage_b,isConiferous,isEvergreen,lightResponseClass)) %>%
  collect() %>%
  mutate(across(c(specificLeafArea:lightResponseClass),as.numeric)) 

dbDisconnect(conn2) # close the connection

# combine
iland.in <- rbind(iland.in, iland.in2)

# merge with tree species lookup
spec.in <- read.csv("processed_data/microclimate_data_prep/tree_species_lookup.csv") %>%
  dplyr::select(-comments) %>%
  left_join(iland.in, by=c("shortName_iLand"="shortName")) %>%
  # update params if superseded
  mutate(isConiferous = ifelse(alt_isConiferous=="iland",isConiferous,alt_isConiferous),
         isEvergreen = ifelse(alt_isEvergreen=="iland",isEvergreen,alt_isEvergreen),
         lightResponseClass = ifelse(alt_stol=="iland",lightResponseClass,alt_stol))

# get plot-level LAI, conifer share, evergreen share, BA-weighted shade tolerance
trees.lai <- trees.out %>%
  # join with iland.in
  left_join(iland.in, by=c("species_short"="shortName")) %>%
  # update params if superseded
  mutate(isConiferous = ifelse(alt_isConiferous=="iland",isConiferous,alt_isConiferous),
         isEvergreen = ifelse(alt_isEvergreen=="iland",isEvergreen,alt_isEvergreen),
         lightResponseClass = ifelse(alt_stol=="iland",lightResponseClass,alt_stol)) %>%
  # force numeric again
  mutate(across(c(isConiferous:lightResponseClass),as.numeric)) %>%
  # calc bmFoliage in kg from DBH in cm
  mutate(bmFoliage = bmFoliage_a * (dbh_cm ^ bmFoliage_b)) %>%
  # multiply by sla (m2/kg) to get leafArea in m2
  mutate(leafArea = specificLeafArea * bmFoliage) %>%
  # divide by plot size
  mutate(leafArea_plot = leafArea/plotsize_m2) %>%
  # sum by plot
  group_by(plot_id) %>%
  summarise(lai = sum(leafArea_plot))

trees.ever <- trees.out %>%
  # join with iland.in
  left_join(iland.in, by=c("species_short"="shortName")) %>%
  # update params if superseded
  mutate(isConiferous = ifelse(alt_isConiferous=="iland",isConiferous,alt_isConiferous),
         isEvergreen = ifelse(alt_isEvergreen=="iland",isEvergreen,alt_isEvergreen),
         lightResponseClass = ifelse(alt_stol=="iland",lightResponseClass,alt_stol)) %>%
  # force numeric again
  mutate(across(c(isConiferous:lightResponseClass),as.numeric)) %>%
  # calculate BA
  mutate(BA = pi*((dbh_cm/200)^2)) %>%
  # add up BA for total
  group_by(plot_id) %>%
  mutate(BAtot=sum(BA,na.rm=TRUE)) %>%
  # BA for evergreen share
  ungroup() %>%
  group_by(plot_id,isEvergreen) %>%
  summarise(BA = sum(BA, na.rm=TRUE), BAtot=mean(BAtot,na.rm=TRUE), prop=BA/BAtot) %>%
  # add evergreen flag
  mutate(ever_flag = ifelse(isEvergreen==0,"deciduous",
                            ifelse(isEvergreen==1,"evergreen",NA))) %>%
  dplyr::select(-c(isEvergreen:BAtot)) %>%
  pivot_wider(names_from="ever_flag",names_prefix="prop_",values_from="prop") %>%
  mutate(across(c(prop_deciduous,prop_evergreen),~replace_na(.,0)))

trees.con <- trees.out %>%
  # join with iland.in
  left_join(iland.in, by=c("species_short"="shortName")) %>%
  # update params if superseded
  mutate(isConiferous = ifelse(alt_isConiferous=="iland",isConiferous,alt_isConiferous),
         isEvergreen = ifelse(alt_isEvergreen=="iland",isEvergreen,alt_isEvergreen),
         lightResponseClass = ifelse(alt_stol=="iland",lightResponseClass,alt_stol)) %>%
  # force numeric again
  mutate(across(c(isConiferous:lightResponseClass),as.numeric)) %>%
  # calculate BA
  mutate(BA = pi*((dbh_cm/200)^2)) %>%
  # add up BA for total
  group_by(plot_id) %>%
  mutate(BAtot=sum(BA,na.rm=TRUE)) %>%
  # BA for coniferous
  group_by(plot_id,isConiferous) %>%
  summarise(BA = sum(BA, na.rm=TRUE), BAtot=mean(BAtot,na.rm=TRUE), prop=BA/BAtot) %>%
  # add evergreen flag
  mutate(ever_flag = ifelse(isConiferous==0,"broadleaved",
                            ifelse(isConiferous==1,"coniferous",NA))) %>%
  dplyr::select(-c(isConiferous:BAtot)) %>%
  pivot_wider(names_from="ever_flag",names_prefix="prop_",values_from="prop") %>%
  mutate(across(c(prop_broadleaved,prop_coniferous),~replace_na(.,0)))

trees.stol <- trees.out %>%
  # join with iland.in
  left_join(iland.in, by=c("species_short"="shortName")) %>%
  # update params if superseded
  mutate(isConiferous = ifelse(alt_isConiferous=="iland",isConiferous,alt_isConiferous),
         isEvergreen = ifelse(alt_isEvergreen=="iland",isEvergreen,alt_isEvergreen),
         lightResponseClass = ifelse(alt_stol=="iland",lightResponseClass,alt_stol)) %>%
  # force numeric again
  mutate(across(c(isConiferous:lightResponseClass),as.numeric)) %>%
  # calculate BA
  mutate(BA = pi*((dbh_cm/200)^2)) %>%
  group_by(plot_id) %>%
  summarise(stol = weighted.mean(lightResponseClass,BA, na.rm=TRUE))

### pull all responses and predictors together
pred.all <- temp.select %>%
  left_join(month.phen, by=c("plot_id","date")) %>%
  left_join(trees.lai, by="plot_id") %>%
  left_join(trees.ever, by="plot_id") %>%
  left_join(trees.con, by="plot_id") %>%
  left_join(trees.stol, by="plot_id") %>%
  left_join(distcoast, by="plot_id") %>%
  left_join(plots.demout, by="plot_id") %>%
  left_join(coords.out[,!names(coords.out) %in% c("elev_m")], by=c("plot_id","data_source")) %>%
  dplyr::select(c(date,plot_id,lon,lat,data_source,micro_minT,micro_maxT,micro_meanT,buff_minT,buff_maxT,buff_meanT,macro_minT,macro_maxT,macro_meanT,snow,excludeFlag_region,excludeFlag_plot,GSI,phenology,lai,prop_evergreen,prop_deciduous,prop_broadleaved,stol,distcoast_km,elev_m,slope_deg,northness,tpi,rel_elev)) %>%
  # remove 2 plots with no trees, these are clearcuts, use lai to filter
  # remove other entries with missing data due to macroclimate (GSI) or microclimate (micro temp)
  filter(!is.na(lai), !is.na(phenology),!is.na(GSI), !is.na(micro_minT)) 

length(unique(pred.all$plot_id)) # 502 plots
summary(pred.all) # no nas for response or predictors

write.csv(pred.all, paste0("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_",name,"_data_daily.csv"),row.names=FALSE)

### for empirical model save dataset to make monthly predictions
# first count flagged values
count.formica <- pred.all %>%
  filter(data_source=="formica") %>%
  mutate(year=year(date),
         month=month(date),
         yearmonth = as.yearmon(paste(year, month), "%Y %m")) %>%
  # date range per Meeussen et al.
  filter(yearmonth>("May 2018"), yearmonth<("Jun 2020")) %>%
  dplyr::select(-yearmonth) 

count.all <- pred.all %>%
  filter(!data_source=="formica") %>%
  mutate(year=year(date),
         month=month(date)) %>%
  rbind(count.formica) 

exclude.sums <- count.all %>%
  mutate(excludeFlag = ifelse(snow == 1 | excludeFlag_region == 1 | excludeFlag_plot == 1, 1, 0)) %>%
  group_by(excludeFlag) %>%
  tally() 

exclude.sums[exclude.sums$excludeFlag==1,]$n/sum(exclude.sums$n)

# omit all flagged values, for snow, region, and plot-level outliers
# then take mean values for everything: micro temp, buffer, macro temp, GSI
pred.monthfull <- pred.all %>%
  # remove flagged values
  filter(snow == 0, excludeFlag_region == 0, excludeFlag_plot == 0) %>%
  mutate(year=year(date),
         month=month(date),
         # add count, so could exclude months with few days represented
         count=1) %>%
  # now agg to month
  group_by(plot_id,data_source,year,month) %>%
  summarise(across(c(micro_minT:rel_elev),~mean(.)),
            count=sum(count)) %>%
  # only keep if at least half the month has values
  filter(count>=15)

summary(pred.monthfull)

write.csv(pred.month, paste0("processed_data/microclimate_data_prep/exploratory_incremental_steps/micro_",name,"_data_monthly.csv"),row.names=FALSE)

# subset monthly data for formica plots, only include 2 yrs from published ms, to avoid too dominant of an influence of these plots (since other datasets have only 1-2 yrs of data)
pred.formica <- pred.monthfull %>%
  filter(data_source=="formica") %>%
  mutate(yearmonth = as.yearmon(paste(year, month), "%Y %m")) %>%
  # date range per Meeussen et al.
  filter(yearmonth>("May 2018"), yearmonth<("Jun 2020")) %>%
  dplyr::select(-yearmonth) 

pred.month <- pred.monthfull %>%
  filter(!data_source=="formica") %>%
  rbind(pred.formica) 

write.csv(pred.month, paste0("processed_data/microclimate_data_prep/micro_macro_data_monthly.csv"),row.names=FALSE)

# clean version, only including final predictors for data deposit
pred.month_clean <- pred.month %>%
  dplyr::select(c(plot_id,data_source,year,month,micro_minT,micro_maxT,buff_minT,buff_maxT,macro_minT,macro_maxT,lai,stol,northness,tpi,count))

write.csv(pred.month_clean, paste0("processed_data/microclimate_data_prep/micro_macro_data_monthly_clean.csv"),row.names=FALSE)
