#####
#
## Preprocess iland outputs for simulation experiment
#
#####

### load libraries
library(tidyverse)
library(terra)
library(RSQLite)
library(cowplot)
library(landscapemetrics) # classify patches

### selected sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] landscapemetrics_2.0.0 cowplot_1.1.1          RSQLite_2.3.3          terra_1.7-55
# [5] lubridate_1.9.3        forcats_1.0.0          stringr_1.5.1          dplyr_1.1.4
# [9] purrr_1.0.2            readr_2.1.4            tidyr_1.3.0            tibble_3.2.1 
# [13] ggplot2_3.4.4          tidyverse_2.0.0    

###
# 1. arguments
###

# # command line arguments
args = commandArgs(TRUE)
scen = as.character(args[1]) # microclimate, macroclimate
rep = as.integer(args[2])

# # trial runs
# scen = "macroclimate"
# scen = "microclimate"
# rep = 1

###
# 2. load dem data
###

### prep dem data for plot comparisons
rid.grid <- rast("iland/gis/objectid.asc")
crs(rid.grid) <- "epsg:31468"

dem.in <- rast("iland/gis/dem10_eu_copernicus.asc")
crs(dem.in) <- "epsg:31468"

preddem.in <- c(rast("iland/gis/TPI.asc"),
                rast("iland/gis/Northness.asc"))
crs(preddem.in) <- "epsg:31468"

dem.ru <- c(crop(dem.in,rid.grid), crop(preddem.in,rid.grid)) %>%
  aggregate(fact=10,fun=mean) %>%
  terra::mask(rid.grid)

dem.df <- c(rid.grid, dem.ru) %>%
  as.data.frame() %>%
  filter(!is.na(objectid)) %>%
  rename(rid=objectid,
         Elev_m = lyr.1)

###
# 3. load and prep iland outputs
###

### load relevant iland outputs
conn=DBI::dbConnect(RSQLite::SQLite(), dbname = paste0("iland/output/simulation_experiment_",scen,"/rep_",rep,"/output_",scen,".sqlite")) # connect to the db
dbListTables(conn)

# stands for lai, composition, regen density
stand.in <-  tbl(conn, "stand") %>%
  dplyr::select(c(year,rid,species,count_ha,basal_area_m2,LAI,cohort_count_ha)) %>%
  filter(year<31) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# sapling density
saps.in <-  tbl(conn, "sapling") %>% 
  mutate(sap_count_ha = count_ha + count_small_ha) %>% 
  dplyr::select(c(year,rid,species,sap_count_ha)) %>%
  filter(year<31) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# carbon for respiration, current year for flow
carbon.in <- tbl(conn, "carbonflow") %>%
  dplyr::select(c(year,rid,Rh)) %>%
  filter(year<31) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# landscape: species composition
land.in <-  tbl(conn, "landscape") %>% 
  dplyr::select(c(year,species,basal_area_m2,cohort_count_ha)) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# landscape: NEP
cflow.in <- tbl(conn, "carbonflow") %>% 
  filter(rid==-1) %>%
  dplyr::select(c(year,Rh,NEP,cumNEP)) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# landscape: total C
cpool.in <- tbl(conn, "carbon") %>% 
  filter(rid==-1) %>%
  # add all pools, no understory C, also sum live woody, dead woody, litter, soil pools
  mutate(live_c_kg = stem_c + branch_c + foliage_c + coarseRoot_c + fineRoot_c + regeneration_c,
         deadWoody_c_kg = snags_c + snagsOther_c + downedWood_c,
         litter_c_kg = litter_c,
         soil_c_kg = soil_c,
         total_c_kg = live_c_kg + deadWoody_c_kg + litter_c_kg + soil_c_kg) %>% 
  dplyr::select(c(year,total_c_kg,live_c_kg,deadWoody_c_kg,litter_c_kg,soil_c_kg)) %>%
  mutate(group=scen,
         rep=rep) %>%
  collect()

# landscape: disturbance effects
beetlesLand.in <- tbl(conn,"barkbeetle") %>% 
  dplyr::select(c(year,killedVolume)) %>%
  mutate(agent="beetles",
         group=scen,
         rep=rep) %>%
  collect()

wind.in <- tbl(conn,"wind") %>%
  dplyr::select(c(year,killedVolume)) %>%
  mutate(agent="wind",
         group=scen,
         rep=rep) %>%
  collect()

dbDisconnect(conn) # close the connection

### load beetle generation grids
rid.nocrs <- rast("iland/gis/objectid.asc")
beetles.in <- c(rid.nocrs,rast(list.files(paste0("iland/output/simulation_experiment_",scen,"/rep_",rep,"/"), full.names=TRUE, pattern="generations"))) %>%
  as.data.frame() %>%
  filter(!is.na(objectid)) %>%
  pivot_longer(-c(objectid)) %>%
  separate(name, into=c("disturbance","name","year"), sep="_") %>%
  rename(rid=objectid, generations=value) %>%
  mutate(group=scen,
         rep=rep,
         year=as.numeric(year)) %>%
  dplyr::select(c(year,rid,generations,group,rep))

### load species parameter lookup for shade tolerance comparison
conn <-
  DBI::dbConnect(RSQLite::SQLite(),
                 dbname = paste0("data/iland_inputs/species_param_europe.sqlite")) # connect to the db
dbListTables(conn)

iland.in <- tbl(conn, "species") %>% 
  dplyr::select(c(shortName,lightResponseClass)) %>%
  collect() %>%
  mutate(across(c(lightResponseClass),as.numeric)) 

dbDisconnect(conn) # close the connection

### additional prep for regen
# total cohort and stem count
regen.all <- stand.in %>%
  left_join(saps.in, by=c("year","rid","group","species","rep")) %>%
  # sap count is na if cohort count is 0, update
  mutate(sap_count_ha = ifelse(is.na(sap_count_ha) & cohort_count_ha==0,0,sap_count_ha)) %>%
  group_by(year,rid,group,rep) %>%
  summarise(cohort_count_ha=sum(cohort_count_ha),
            sap_count_ha = sum(sap_count_ha))

# species specific sapling stem density
regen.allstems <- stand.in %>%
  left_join(saps.in, by=c("year","rid","group","species","rep")) %>%
  dplyr::select(c(year,rid,group,rep,species,sap_count_ha,group)) %>%
  pivot_wider(names_from="species",values_from="sap_count_ha", names_prefix = "sap_count_ha_") %>%
  mutate(across(c(sap_count_ha_piab:sap_count_ha_tipl),~replace_na(.,0)))

###
# 4. local scale pre-processing
###

### H: We expect microclimate temperature buffering to decrease heterotrophic respiration, dampen bark beetle outbreaks, and maintain similar tree regeneration densities in dense, mature forested stands (characterized by tall trees with high LAI, comparable composition and topographic position). 
# Look at first 30 years of each replicate and comparing processes of interest between microclimate and macroclimate runs in closed canopy stands (LAI > 4 ,von Arx et al. 2013 J Ecology, used to define dense canopy); use LAI from previous year to classify, because this is what affects subsequent year offset, processes

# subset to dense stands, add shade tolerance
stand.dense <- stand.in %>%
  # join with shade tolerance
  left_join(iland.in, by=c("species"="shortName")) %>%
  # will match up with subsequent year for process effects
  mutate(year=year+1) %>%
  filter(year<31) %>%
  group_by(year,rid,group,rep) %>%
  summarise(LAI=sum(LAI),
            STol = weighted.mean(lightResponseClass,basal_area_m2, na.rm=TRUE)) %>%
  # filter for closed canopy, dense, use LAI>4
  filter(LAI>4) 

# add topo for predictor comparison
# add indicators for decomp, beetles, regen
stand.comb <- stand.dense %>%
  left_join(dem.df, by=c("rid")) %>%
  left_join(carbon.in, by=c("year","rid","group","rep")) %>%
  left_join(beetles.in, by=c("year","rid","group","rep")) %>%
  left_join(regen.all, by=c("year","rid","group","rep")) %>%
  left_join(regen.allstems, by=c("year","rid","group","rep")) %>%
  # if nas present, set to 0, these areas potentially disturbed, still include
  mutate(across(c(cohort_count_ha:sap_count_ha_tipl),~replace_na(.,0))) 

write.csv(stand.comb, paste0("processed_data/simulation_experiment/localScale_denseStands_",scen,"_rep_",rep,".csv"), row.names=FALSE)

###
# 5. meso scale pre-processing
###

### Disturbance patches. We expect microclimate temperature buffering to enhance differences in decomposition rates, beetle development rates, and tree regeneration composition between post- and pre-disturbance patches (wind or beetle).
# here consider disturbances within first 10 years, aggregate to single event to acknowledge that some processes such as beetles play out over multiple years
# just look at processes same pixels pre- and post-disturbance (for post use 5-15 yrs post-disturbance, peak temp buffering effects per Vandewiele et al. 2023)

# load events
dist.events <- sum(crop(rast(paste0("iland/output/simulation_experiment_",scen,"/rep_",rep,"/beetle_events_10.asc")),rid.nocrs),
                   crop(rast(paste0("iland/output/simulation_experiment_",scen,"/rep_",rep,"/wind_events_10.asc")),rid.nocrs)) 

# code as binary, 0 or 1
dist.events[dist.events>1] = 1

# aggregate to 100m resolution to align with ru grid
dist.ru <- aggregate(dist.events,fact=10,fun=mean) 

# code as binary again, if at least half the ru experienced disturbance
dist.ru[dist.ru>=0.5] <- 1
dist.ru[dist.ru<0.5] <- 0
plot(dist.ru)

# classify patches based on 8-nbr rule
dist.patch <- get_patches(dist.ru, class=1, directions=8)

# outputs as data frame, only disturbance patches
patch.df <- c(rid.nocrs,dist.patch$layer_1$class_1) %>%
  as.data.frame() %>%
  rename(patch=lyr.1) %>%
  filter(!is.na(patch))

# add processes and subset year 0 or 1 depending on process (pre) and year 16 (5-15 yr post)
stand.select <- carbon.in %>%
  # years 1 and 16 for pre- and post-disturbance for carbon and beetle generations
  filter(rid %in% c(patch.df$objectid),
         year %in% c(1,16)) %>%
  left_join(beetles.in, by=c("year","rid","group","rep")) %>%
  # years 0 and 16 for pre- and post-disturbance for regen
  mutate(year=ifelse(year==1, year-1,year)) %>%
  left_join(regen.all, by=c("year","rid","group","rep")) %>%
  left_join(regen.allstems, by=c("year","rid","group","rep")) %>%
  filter(group==scen) %>%
  mutate(disturbance = ifelse(year==0,"predisturbance",
                              ifelse(year==16,"postdisturbance",NA))) %>%
  left_join(patch.df, by=c("rid"="objectid")) %>%
  dplyr::select(c(year,rid,group,rep,disturbance,patch,Rh,generations:sap_count_ha_tipl))


# save data
write.csv(stand.select, paste0("processed_data/simulation_experiment/mesoScale_disturbancePatches_",scen,"_rep_",rep,".csv"),row.names=FALSE)

### Reneration composition. We expect microclimate temperature buffering to have greater effects on tree regeneration composition and species-specific regeneration density close to the thermal edges of species ranges and regeneration niches (e.g., forest type ecotones, higher elevations for fir and beech).

# consider subset of species: fasy, abal, acps, piab, pice, lade; temp preference from Ellenberg (EIV)
# warm-pref/submontane-montane: fasy (5), abal (5)
# cool-pref/subalpine: piab (3)
# cold-pref: subalpine-alpine: pice (2)
# indifferent: lade, acps

# thermal range from Ewald 2012, altitudinal tree species distrib in Bavarian Alps, based on database of forest inventory plots; 100 m belt centered on approx location of lower whisker (lower edge), median (core), and upper whisker (upper edge) for regeneration layer, only include if midpoint within elevation range in landscape (600-2100m); extracted from plot digitizer
# fasy lower: NA, core: 1060, upper: 1570
# abal lower: 630 but use 650 to get full 100 m belt, core: 1110, upper: 1650
# acps lower: NA, core: 1110, upper: 1780
# piab lower: NA, core: 1120, upper: 1950
# lade lower: 620 but use 650 to get full 100 m belt, core: 1410, upper: 1820
# pice lower: 1350, core: 1620, upper: 1820

# compare with observed ecotone in landscape, only save fig for 1st rep
if(rep==1) {
  
  ecotone.plot <- regen.allstems %>%
    dplyr::select(c(year,rid,group,sap_count_ha_fasy,sap_count_ha_abal,sap_count_ha_acps,sap_count_ha_piab,sap_count_ha_lade,sap_count_ha_pice)) %>%
    pivot_longer(c(sap_count_ha_fasy:sap_count_ha_pice))%>%
    left_join(dem.df, by="rid") %>%
    group_by(name,Elev_m) %>%
    summarise(value=mean(value)) %>%
    ggplot(aes(x=Elev_m,y=value)) +
    facet_wrap(~name,scales="free_y") +
    geom_point() +
    geom_smooth(method="loess") +
    theme_bw()
  
  ggsave(plot=ecotone.plot, filename=paste0("figures/simulation_experiment_checks/q1_2_ecotone_check_",scen,"_rep_",rep,".png"),width=6,height=4)
}

# fasy lower: NA, core: 1060, upper: 1570
# abal lower: 630 but use 650 to get full 100 m belt, core: 1110, upper: 1650
# acps lower: NA, core: 1110, upper: 1780
# piab lower: NA, core: 1120, upper: 1950
# lade lower: 620 but use 650 to get full 100 m belt, core: 1410, upper: 1820
# pice lower: 1350, core: 1620, upper: 1820

ecotone.lookup <- rbind(
  # set lower to 0 if NA
  data.frame(name="sap_count_ha_fasy",lower=0, core=1060, upper=1570),
  data.frame(name="sap_count_ha_abal",lower=650, core=1110, upper=1650),
  data.frame(name="sap_count_ha_acps",lower=0,core=1110,upper=1780),
  data.frame(name="sap_count_ha_piab",lower=0,core=1120,upper=1950),
  data.frame(name="sap_count_ha_lade",lower=650,core=1410,upper=1820),
  data.frame(name="sap_count_ha_pice",lower=1350,core=1620,upper=1820)
)

regen.ecotone <- regen.allstems %>%
  # remove year 0, identical in micro and macro runs
  filter(year>0) %>%
  # subset and join with other dfs
  dplyr::select(c(year,rid,group,rep,sap_count_ha_fasy,sap_count_ha_abal,sap_count_ha_acps,sap_count_ha_piab,sap_count_ha_lade,sap_count_ha_pice)) %>%
  pivot_longer(c(sap_count_ha_fasy:sap_count_ha_pice))%>%
  left_join(regen.all, by=c("year","rid","group","rep")) %>%
  left_join(dem.df, by="rid") %>%
  left_join(ecotone.lookup, by=c("name")) %>%
  # code thermal range position
  mutate(ecotone = ifelse(Elev_m>lower-50 & Elev_m<lower+50,"lower",
                          ifelse(Elev_m>core-50 & Elev_m<core+50,"core",
                                 ifelse(Elev_m>upper-50 & Elev_m<upper+50,"upper",NA)))) %>%
  # only keep relevant thermal ranges
  filter(!is.na(ecotone)) %>%
  rename(sap_count_ha_all=sap_count_ha) %>%
  dplyr::select(c(year,rid,group,rep,ecotone,Elev_m,name,value,sap_count_ha_all))

# save
write.csv(regen.ecotone, paste0("processed_data/simulation_experiment/mesoScale_thermalRangeRegenStems_",scen,"_rep_",rep,".csv"),row.names=FALSE)


###
# 6. landscape scale pre-processing
###

### We expect microclimate temperature buffering to increase total carbon (live + dead) and net ecosystem productivity (NEP) at the landscape scale. 
# also consider individual c pools
# include stand scale indicator: Rh

# combine carbon and nep
carbon.out <- cflow.in %>%
  left_join(cpool.in, by=c("year","group","rep")) %>%
  dplyr::select(c(year,group,rep,Rh,NEP,cumNEP,total_c_kg:soil_c_kg))

# include stand scale indicator: beetle generations across entire landscape, first 30 simulation years
generations.land <- beetles.in %>%
  group_by(year,group,rep) %>%
  summarise(generations=mean(generations))

# disturbances
dist.out <- beetlesLand.in %>%
  rbind(wind.in) %>%
  pivot_wider(names_from="agent",values_from="killedVolume",names_prefix="killedVolume_") %>%
  mutate(across(c(killedVolume_beetles,killedVolume_wind),~replace_na(.,0))) %>%
  # add beetle generations
  left_join(generations.land, by=c("year","group","rep"))

# write out
write.csv(carbon.out, paste0("processed_data/simulation_experiment/landscapeScale_carbon_",scen,"_rep_",rep,".csv"),row.names=FALSE)
write.csv(dist.out, paste0("processed_data/simulation_experiment/landscapeScale_disturbances_",scen,"_rep_",rep,".csv"),row.names=FALSE)

### We expect similar forest composition regardless of temperature buffering because tree species are adapted to historical climate conditions and temperature filters are less important for determining species occurrence at landscape scales compared to light and seed availability.
# species composition
# include stand scale indicator: sap_count_ha, for first 30 years
regen.land <- regen.all %>%
  left_join(regen.allstems, by=c("year","rid","group","rep")) %>%
  dplyr::select(-c(cohort_count_ha,sap_count_ha)) %>%
  group_by(year,group,rep) %>%
  summarise(across(c(sap_count_ha_piab:sap_count_ha_tipl), mean)) %>%
  pivot_longer(c(sap_count_ha_piab:sap_count_ha_tipl),names_to = "species", names_prefix = "sap_count_ha_",values_to="sap_count_ha")

land.out <- land.in %>%
  left_join(regen.land, by=c("year","group","rep","species")) %>%
  dplyr::select(c(year,species,basal_area_m2,cohort_count_ha,sap_count_ha,group,rep))
  
# write out
write.csv(land.out, paste0("processed_data/simulation_experiment/landscapeScale_composition_",scen,"_rep_",rep,".csv"),row.names=FALSE)

