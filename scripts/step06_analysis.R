#####
#
## Analyses for temp buffering eval, simulation experiment, sensitivity analysis
#
#####

### load libraries
library(tidyverse)
library(terra)
library(ggpubr)
library(plotrix) # std.error
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
#   [1] RSQLite_2.3.3   plotrix_3.8-4   ggpubr_0.6.0    terra_1.7-55    lubridate_1.9.3
# [6] forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2     readr_2.1.4    
# [11] tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.4   tidyverse_2.0.0

###
# 1. characterize temperature buffering and drivers across landscape, by forest type
###

### load dem and predictors
dem.in <- rast("iland/gis/dem10_eu_copernicus.asc")
crs(dem.in) <- "epsg:31468"
names(dem.in) <- "Elev_m"

pred.in <- c(rast("iland/output/temp_buffering_microclimate/TPI.asc"),
             rast("iland/output/temp_buffering_microclimate/Northness.asc"),
             rast("iland/output/temp_buffering_microclimate/LAI_1.asc"),
             rast("iland/output/temp_buffering_microclimate/STol_1.asc"))
crs(pred.in) <- "epsg:31468"

# load standgrid
standid.in <- rast("processed_data/iland_simulation_prep/stand_id_raster.tif")
crs(standid.in) <- "epsg:31468"

dem.df <- c(crop(dem.in,standid.in),crop(pred.in,standid.in), standid.in) %>%
  as.data.frame() %>%
  filter(!is.na(lyr.1), lyr.1>0)

### read in climate data for 1988
clim.in <- read.csv("processed_data/iland_simulation_prep/climate_table_monthly.csv") %>%
  filter(year==1988)

# number of days per month
monthdays <- clim.in %>%
  dplyr::select(month,days) %>%
  distinct()

# env data to match up with rid
env.in <- read.table("iland/gis/environment.txt", header=TRUE) %>%
  dplyr::select(c(id,model.climate.tableName))

### read in and summarize 10m buffering data
buff.10m <- read.csv("iland/output/temp_buffering_microclimate/monthly_buffering_1988_10m.csv") 

# seasonal, use weighted mean to account for different number of days per month
buff10m.seas <- buff.10m %>% 
  left_join(monthdays, by="month") %>%
  group_by(rid,standid,forest_types2020_iland,forest_type_eng,year,season) %>%
  summarise(across(c(buff_maxT,buff_minT,buff_meanT),~weighted.mean(.,days)))

# annual, use weighted mean to account for different number of days per month
buff10m.ann <- buff.10m %>% 
  left_join(monthdays, by="month") %>%
  group_by(rid,standid,forest_types2020_iland,forest_type_eng,year) %>%
  summarise(across(c(buff_maxT,buff_minT,buff_meanT),~weighted.mean(.,days))) %>%
  mutate(season="annual")

# mean, se, median, sd, range by season, full landscape
buff.landscape <- buff10m.seas %>%
  rbind(buff10m.ann) %>%
  group_by(year,season) %>%
  summarise(across(c(buff_maxT,buff_minT,buff_meanT),list(median=median,mean=mean,se=std.error,sd=sd,min=min,max=max)))

# mean, se, median, sd, range by forest type and season
buff.forestType <- buff10m.seas %>%
  rbind(buff10m.ann) %>%
  group_by(year,season,forest_type_eng) %>%
  summarise(across(c(buff_maxT,buff_minT,buff_meanT),list(median=median,mean=mean,se=std.error,sd=sd,min=min,max=max)))

# look at trend over elevational/topographic gradient, summer only, use sample of 5% of values for quicker plotting
buff.10m %>%
  filter(season=="summer") %>%
  slice_sample(n=43000) %>%
  left_join(dem.df, by=c("standid"="lyr.1")) %>%
  pivot_longer(c(buff_maxT,buff_meanT,buff_minT)) %>%
  pivot_longer(c(Elev_m,TPI,Northness),names_to="pred",values_to="val2") %>%
  ggplot(aes(x=val2, y=value)) +
  facet_grid(name~pred, scales="free_x") +
  geom_point() +
  geom_smooth(method="lm",se=FALSE) +
  stat_cor(
    method="spearman",cor.coef.name="rho",p.digits=NA, label.sep=""
  ) +
  theme_bw()

# write out averages
write.csv(buff.landscape,"analysis/temp_buffering_microclimate/buffer_landscape.csv",row.names=FALSE)
write.csv(buff.forestType,"analysis/temp_buffering_microclimate/buffer_forestType.csv",row.names=FALSE)

### also summarize mean annual driver data, macro and microclimate
clim.ann <- clim.in %>%
  group_by(table_name) %>%
  summarise(across(c(min_temp,max_temp,mean_temp),~weighted.mean(.,days))) %>%
  rename(macro_minT = min_temp,
         macro_maxT = max_temp,
         macro_meanT = mean_temp)

drivers.ann <- buff10m.ann %>%
  left_join(dem.df, by=c("standid"="lyr.1")) %>%
  left_join(env.in, by=c("rid" = "id")) %>%
  left_join(clim.ann, by=c("model.climate.tableName" = "table_name")) %>%
  # calc mean annual microclimate temps
  mutate(micro_minT = macro_minT + buff_minT,
         micro_meanT = macro_meanT + buff_meanT,
         micro_maxT = macro_maxT + buff_maxT) %>%
  pivot_longer(c(buff_maxT,buff_minT,buff_meanT,TPI,Northness,LAI_1,STol_1,macro_minT:micro_maxT)) %>%
  group_by(year,season,name) %>%
  summarise(across(value,list(mean=mean,sd=sd,min=min,max=max)))

# write out
write.csv(drivers.ann,"analysis/temp_buffering_microclimate/driver_summary.csv",row.names=FALSE)

### compare with independent data
# create raster with summer mean temp offset
buff10m.summ <- buff10m.seas %>%
  filter(season=="summer")

meanT.rast <- classify(standid.in,rcl = cbind(buff10m.summ$standid,buff10m.summ$buff_meanT))

# write out
writeRaster(meanT.rast, "processed_data/temp_buffering_microclimate/simulated_summerMeanTOffset.tif")

# read in comparison data, Vandewiele et al. 2023
vand.in <- rast("data/microclimate_data/Vandewiele_etal2023/predictions/prediction_TO_mean.tif")

# compare rasters with spearman's rank correlation
# reproject comparison data
vand.project <- vand.in %>%
  project(y=crs(meanT.rast), method="near")

# raster to points for finer resolution raster (simulated)
meanT.pts <- as.points(meanT.rast)

# extract data from coarser resolution raster (indep)
meanT.vand <- extract(vand.project, meanT.pts)
meanT.match <- cbind(meanT.pts$lyr.1, meanT.vand$lyr1) %>%
  as.data.frame() %>%
  rename(iland = V1, vand = V2) %>%
  # remove NAs
  filter(!is.na(vand))

# quick plot of sample of points
meanT.match %>%
  slice_sample(n=1000) %>%
  ggplot(aes(x=vand, y=iland)) +
  geom_point() +
  geom_smooth(method="lm") +
  stat_cor(method="spearman") +
  theme_bw()

# correlation of full dataset
cor(meanT.match$iland, meanT.match$vand, method="spearman")

###
# 2. Local scale analyses
###

### H: We expect microclimate temperature buffering to decrease heterotrophic respiration, dampen bark beetle outbreaks, and maintain similar tree regeneration densities in dense, mature forested stands (characterized by tall trees with high LAI, comparable composition and topographic position). 
# Look at first 30 years of each replicate and comparing processes of interest between microclimate and macroclimate runs in closed canopy stands (LAI > 4 ,von Arx et al. 2013 J Ecology, used to define dense canopy); use LAI from previous year to classify, because this is what affects subsequent year offset, processes

### load pre-processed iland outputs
stand.comb <- list.files(path="processed_data/simulation_experiment/", pattern="localScale_denseStands", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

# compare forest and topo characteristics for dense stands
stand.comb %>%
  pivot_longer(c(LAI:Northness)) %>%
  ggplot(aes(x=group,y=value,group=group, fill=group)) +
  facet_wrap(~name, scales="free") +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank()
  ) 

ggsave(filename=paste0("figures/simulation_experiment_checks/q1_1_dense_stand_predictor_comparison.png"),width=6,height=4)

# how many stands total
stand.ns <- stand.comb %>%
  group_by(group) %>%
  tally()
print(paste0("macroclimate dense stands =  ", stand.ns[stand.ns$group=="macroclimate",]$n, ", microclimate dense stands = ", stand.ns[stand.ns$group=="microclimate",]$n))
883983/856841 # more stands with microclimate

### data for analysis and plotting
# average by replicate
standDense.reps <- stand.comb %>%
  group_by(rep,group) %>%
  summarise(across(c(Rh:sap_count_ha_tipl), mean)) %>%
  # convert Rh to Mg/ha
  mutate(Rh = abs(Rh)/1000) %>%
  # rename
  rename(Rh_Mg_ha = Rh,
         beetle_generations_ha = generations)

# summary stats across replications
standDense.summary <- standDense.reps %>%
  pivot_longer(c(Rh_Mg_ha:sap_count_ha_tipl)) %>%
  pivot_wider(names_from="group",values_from="value") %>%
  # add tally for macro > micro
  mutate(macro_higher = ifelse(macroclimate>microclimate,1,0)) %>%
  # summary stats
  group_by(name) %>%
  summarise(across(c(macroclimate,microclimate), list(mean=mean,se=std.error)), 
            macro_higher=sum(macro_higher)) %>%
  # add relative difference just between means
  mutate(relDiff=100*(microclimate_mean - macroclimate_mean)/macroclimate_mean)

# write out rep and summary data
write.csv(standDense.reps, "analysis/simulation_experiment/localScale_denseStands_allReps.csv",row.names=FALSE)
write.csv(standDense.summary, "analysis/simulation_experiment/localScale_denseStands_summaryStats.csv",row.names=FALSE)

### compare regen composition based on proportion for focal common species
standDense.summary %>%
  pivot_longer(c(macroclimate_mean:microclimate_se),names_sep="_",names_to=c("group","stat")) %>%
  pivot_wider(names_from="stat",values_from="value") %>%
  filter(grepl("sap_count",name)) %>%
  # focal species
  mutate(name=ifelse(name%in% c("sap_count_ha_abal","sap_count_ha_fasy","sap_count_ha_lade","sap_count_ha_piab","sap_count_ha_pimu","sap_count_ha_pice","sap_count_ha_acps","sap_count_ha_frex","sap_count_ha_soau","sap_count_ha_saca","sap_count_ha_soar","sap_count_ha"),name,"sap_count_ha_other")) %>%
  # add up count for other
  group_by(group,name) %>%
  summarise(across(mean,sum)) %>%
  pivot_wider(names_from="name",values_from="mean") %>%
  # proportion
  mutate(across(c(sap_count_ha_abal:sap_count_ha_soau),~./sap_count_ha)) %>%
  pivot_longer(c(sap_count_ha_abal:sap_count_ha_soau), values_to="prop") %>%
  dplyr::select(-sap_count_ha) %>%
  pivot_wider(names_from="group", values_from="prop") %>%
  mutate(diff = 100*(microclimate-macroclimate)) %>%
  arrange(diff)


###
# 3. Meso scale analyses
###

### Disturbance patches. We expect microclimate temperature buffering to enhance differences in decomposition rates, beetle development rates, and tree regeneration composition between post- and pre-disturbance patches (wind or beetle).
# here consider disturbances within first 10 years, aggregate to single event to acknowledge that some processes such as beetles play out over multiple years
# just look at processes same pixels pre- and post-disturbance (for post use 5-15 yrs post-disturbance, peak temp buffering effects per Vandewiele et al. 2023)

### load pre-processed iland outputs
stand.select <- list.files(path="processed_data/simulation_experiment/", pattern="mesoScale_disturbancePatches", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

### data for analysis and plotting
# summary across reps, for each patch
distPatch.patches <- stand.select %>%
  # convert Rh to Mg/ha
  mutate(Rh = abs(Rh)/1000) %>%
  # rename
  rename(Rh_Mg_ha = Rh,
         beetle_generations_ha = generations) %>%
  # add rep
  mutate(rep=rep) %>%
  # add count for patch size
  mutate(area_ha = 1) %>%
  group_by(rep,group,disturbance,patch) %>%
  summarise(across(c(Rh_Mg_ha,beetle_generations_ha,sap_count_ha:sap_count_ha_tipl),mean), area_ha = sum(area_ha))

# summary across patches
distPatch.summary <- distPatch.patches %>%
  pivot_longer(c(Rh_Mg_ha:sap_count_ha)) %>%
  dplyr::select(c(rep,group,disturbance,patch,name,value)) %>%
  pivot_wider(names_from="disturbance",values_from="value") %>%
  # post minus pre, agreement among patches
  mutate(disturbance_change = postdisturbance - predisturbance,
         disturbance_increase = ifelse(postdisturbance>predisturbance,1,0)) %>%
  # add count for number of patches
  mutate(n_patch = 1) %>%
  group_by(group,name) %>%
  summarise(across(c(disturbance_change),list(median = median,
                                 mean= mean,
                                 se = std.error)),
            n_patch=sum(n_patch),
            disturbance_increase = 100*sum(disturbance_increase)/n_patch)

# compare patch sizes
distPatch.patches %>%
  filter(disturbance=="postdisturbance",group=="macroclimate") %>%
  dplyr::select(area_ha) %>%
  summary()

distPatch.patches %>%
  filter(disturbance=="postdisturbance",group=="microclimate") %>%
  dplyr::select(area_ha) %>%
  summary()

# pooled summaries across all disturbance patches
distPatch.patches %>%
  pivot_longer(c(Rh_Mg_ha:sap_count_ha)) %>%
  dplyr::select(c(rep,group,disturbance,patch,name,value)) %>%
  pivot_wider(names_from="disturbance",values_from="value") %>%
  # post minus pre, agreement among patches
  mutate(disturbance_change = postdisturbance - predisturbance) %>%
  ungroup() %>%
  dplyr::select(c(rep,group,patch,name,disturbance_change)) %>%
  pivot_wider(names_from="name",values_from="disturbance_change") %>%
  summary()

# write out disturbance patch and summary data
write.csv(distPatch.patches, "analysis/simulation_experiment/mesoScale_disturbancePatches_allPatches.csv",row.names=FALSE)
write.csv(distPatch.summary, "analysis/simulation_experiment/mesoScale_disturbancePatches_summary.csv",row.names=FALSE)


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

### load pre-processed iland outputs
regen.ecotone <- list.files(path="processed_data/simulation_experiment/", pattern="mesoScale_thermalRangeRegenStems", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

### ensure complete cases between micro and macro scenarios
regen.complete <- regen.ecotone %>%
  dplyr::select(-c(sap_count_ha_all)) %>%
  pivot_wider(names_from="group",values_from="value") %>%
  # add 0s if NA
  mutate(across(c(macroclimate,microclimate),~replace_na(.,0)))

summary(regen.complete)
regen.complete %>%
  pivot_longer(c(macroclimate,microclimate),names_to="group") %>%
  group_by(ecotone,name,group) %>%
  # group_by(ecotone,group) %>%
  # group_by(name,group) %>%
  tally()

### summarise effect of microclimate
# by rep
ecotone.reps <- regen.complete %>%
  pivot_longer(c(macroclimate,microclimate),names_to="group") %>%
  group_by(group,rep,ecotone,name) %>%
  summarise(value=mean(value)) %>%
  pivot_wider(names_from="group",values_from="value") %>%
  # relative diff
  mutate(relDiff = 100*(microclimate-macroclimate)/macroclimate)

# summary
ecotone.summary <- ecotone.reps %>%
  group_by(ecotone,name) %>%
  summarise(across(c(macroclimate,microclimate,relDiff),list(mean=mean,se=std.error)))

# write out rep and summary data
write.csv(ecotone.reps,"analysis/simulation_experiment/mesoScale_thermalRangeRegen_allReps.csv",row.names=FALSE)
write.csv(ecotone.summary,"analysis/simulation_experiment/mesoScale_thermalRangeRegen_summary.csv",row.names=FALSE)

# overall average, relative sensitivity among species
ecotone.reps %>%
  group_by(name) %>%
  summarise(across(relDiff,mean)) %>%
  mutate(relDiff = abs(relDiff)) %>%
  arrange(relDiff)

ecotone.reps %>%
  group_by(ecotone,name) %>%
  summarise(across(relDiff,mean)) %>%
  mutate(relDiff=abs(relDiff)) %>%
  arrange(ecotone,relDiff)


###
# 4. Landscape scale analyses
###

### We expect microclimate temperature buffering to increase total carbon (live + dead) and net ecosystem productivity (NEP) at the landscape scale.
### We expect similar forest composition regardless of temperature buffering because tree species are adapted to historical climate conditions and temperature filters are less important for determining species occurrence at landscape scales compared to light and seed availability.
# summarize with first and last 30 simulation years, year 30 and 1000 for cumulative
# also include individual c pools, cumulative disturbance mortality, stand scale indicators

### load pre-processed iland outputs
# carbon
carbon.comp <- list.files(path="processed_data/simulation_experiment/", pattern="landscapeScale_carbon", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

# disturbances
dist.comp <- list.files(path="processed_data/simulation_experiment/", pattern="landscapeScale_disturbances", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

# species composition
forest.comp <- list.files(path="processed_data/simulation_experiment/", pattern="landscapeScale_composition", full.names=TRUE) %>%
  map(read_csv) %>%
  list_rbind()

### summarise all landscape indicators over time, all reps; for plotting
carbon.yr <- carbon.comp %>%
  group_by(year,group) %>%
  summarise(across(c(NEP:soil_c_kg),mean)) %>%
  # change all C indicators to Mg/ha
  mutate(across(c(NEP,cumNEP,total_c_kg:soil_c_kg), ~./1000)) %>%
  rename(NEP_Mg_ha=NEP,
         cumNEP_Mg_ha=cumNEP,
         total_C_Mg_ha=total_c_kg,
         live_C_Mg_ha=live_c_kg,
         deadWoody_C_Mg_ha=deadWoody_c_kg,
         litter_C_Mg_ha=litter_c_kg,
         soil_C_Mg_ha=soil_c_kg)

dist.yr <- dist.comp %>%
  # add total disturbance mortality from both agents
  mutate(killedVolume_total = killedVolume_beetles + killedVolume_wind) %>%
  # cumulative sum
  group_by(group,rep) %>%
  mutate(across(c(killedVolume_beetles,killedVolume_wind,killedVolume_total), ~cumsum(.))) %>%
  # avg across reps by yr
  ungroup() %>%
  group_by(year,group) %>%
  summarise(across(c(killedVolume_beetles,killedVolume_wind,killedVolume_total),mean)) 

forest.yr <- forest.comp %>%
  # remove year 0, consistent with other outputs
  filter(year>0) %>%
  group_by(year,group,species) %>%
  summarise(ba_m2_ha=mean(basal_area_m2)) %>%
  pivot_wider(names_from="species",values_from="ba_m2_ha",names_prefix="ba_m2_ha_")

# combine and write out
land.yr <- carbon.yr %>%
  left_join(dist.yr, by=c("year","group")) %>%
  left_join(forest.yr, by=c("year","group"))

write.csv(land.yr, "analysis/simulation_experiment/landscapeScale_byYear.csv",row.names=FALSE)

### early and late landscape indicator summaries, relative differences
# cumulative NEP for year 30 and 1000 only, because it is cumulative
cumNEP.reps <- carbon.comp %>%
  filter(year %in% c(30, 1000)) %>%
  # summarise by rep
  mutate(period = ifelse(year %in% c(30),"early",
                         ifelse(year %in% c(1000),"late",NA))) %>%
  dplyr::select(c(period,group,rep,cumNEP))

# cumulative disturbance mortality
dist.reps <- dist.comp %>%
  # add total disturbance mortality from both agents
  mutate(killedVolume_total = killedVolume_beetles + killedVolume_wind) %>%
  # cumulative sum
  group_by(group,rep) %>%
  mutate(across(c(killedVolume_beetles,killedVolume_wind,killedVolume_total), ~cumsum(.))) %>%
  # years 30 and 1000
  filter(year %in% c(30, 1000)) %>%
  # summarise by rep
  mutate(period = ifelse(year %in% c(30),"early",
                         ifelse(year %in% c(1000),"late",NA))) %>%
  # rename these to indicate cumulative
  rename(cumKilledVolume_beetles=killedVolume_beetles,
         cumKilledVolume_wind=killedVolume_wind,
         cumKilledVolume_total=killedVolume_total) %>%
  dplyr::select(c(period,group,rep,cumKilledVolume_beetles,cumKilledVolume_wind,cumKilledVolume_total))

# forest ba and stem count by species
forest.ba <- forest.comp %>%
  filter(year>0) %>%
  dplyr::select(c(year,species,basal_area_m2,group,rep)) %>%
  pivot_wider(names_from="species",values_from="basal_area_m2",names_prefix="ba_m2_ha_")

# forest regen, only for focal species for local v. landscape comparison
forest.regen <- forest.comp %>%
  filter(year>0, species %in% c("fasy","piab")) %>%
  dplyr::select(c(year,species,sap_count_ha,group,rep)) %>%
  pivot_wider(names_from="species",values_from="sap_count_ha",names_prefix="sap_count_ha_")

# first and last 30 simulation years by rep
land.reps <- carbon.comp %>%
  # drop cumNEP from here
  dplyr::select(-cumNEP) %>%
  # add disturbance indicators
  left_join(dist.comp, by=c("group","rep","year")) %>%
  # add total disturbance mortality from both agents
  mutate(killedVolume_total = killedVolume_beetles + killedVolume_wind) %>%
  # add species composition
  left_join(forest.ba, by=c("group","rep","year")) %>%
  left_join(forest.regen, by=c("group","rep","year")) %>% 
  filter(year %in% c(1:30, 971:1000)) %>%
  # change Rh to pos
  mutate(Rh = abs(Rh)) %>%
  # summarise by rep
  mutate(period = ifelse(year %in% c(1:30),"early",
                         ifelse(year %in% c(971:1000),"late",NA))) %>%
  # add nep and cumulative disturbance mortality
  left_join(cumNEP.reps, by=c("period","group","rep")) %>%
  left_join(dist.reps, by=c("period","group","rep")) %>%
  group_by(period,group,rep) %>%
  summarise(across(c(Rh,NEP,cumNEP,total_c_kg:cumKilledVolume_total), mean)) %>%
  # change all C indicators to Mg/ha
  mutate(across(c(Rh,NEP,cumNEP,total_c_kg:soil_c_kg), ~./1000)) %>%
  rename(Rh_Mg_ha=Rh,
         NEP_Mg_ha=NEP,
         cumNEP_Mg_ha=cumNEP,
         total_C_Mg_ha=total_c_kg,
         live_C_Mg_ha=live_c_kg,
         deadWoody_C_Mg_ha=deadWoody_c_kg,
         litter_C_Mg_ha=litter_c_kg,
         soil_C_Mg_ha=soil_c_kg,
         beetle_generations_ha=generations) 


# summary
land.summary <- land.reps %>%
  pivot_longer(c(Rh_Mg_ha:cumKilledVolume_total)) %>%
  pivot_wider(names_from="group",values_from="value") %>%
  # add differences, relative differences between micro and macro
  mutate(diff = (microclimate-macroclimate),
         relDiff = 100*diff/macroclimate) %>%
  # summarise
  group_by(period,name) %>%
  summarise(across(c(macroclimate,microclimate,diff,relDiff),list(mean=mean,se=std.error)))
  
# rel diff will be NA for species if mean macroclimate indicator is 0

# write out
write.csv(land.summary, "analysis/simulation_experiment/landscapeScale_summary.csv",row.names=FALSE)

### which carbon pools drive differences between micro and macro
land.summary %>%
  filter(period=="late",
         name %in% c("total_C_Mg_ha","live_C_Mg_ha","deadWoody_C_Mg_ha","litter_C_Mg_ha","soil_C_Mg_ha")) %>%
  dplyr::select(name,diff_mean) %>%
  pivot_wider(names_from="name",values_from="diff_mean") %>%
  mutate(across(c(deadWoody_C_Mg_ha:soil_C_Mg_ha)))

### basal area share
land.summary %>%
  filter(period=="late",
         grepl("ba_m2_ha",name)) %>%
  dplyr::select(period,name,macroclimate_mean,microclimate_mean) %>%
  # add total ba
  group_by(period) %>%
  mutate(macroclimate_tot_ba = sum(macroclimate_mean),
         microclimate_tot_ba = sum(microclimate_mean)) %>%
  # ba share by species
  ungroup() %>%
  mutate(macroclimate_ba_share = 100*macroclimate_mean/macroclimate_tot_ba,
         microclimate_ba_share = 100*microclimate_mean/microclimate_tot_ba,
         diff_ba_share = (microclimate_ba_share-macroclimate_ba_share)) %>%
  filter(name %in% c("ba_m2_ha_fasy","ba_m2_ha_piab","ba_m2_ha_abal","ba_m2_ha_lade","ba_m2_ha_pice")) %>%
  dplyr::select(c(name,macroclimate_ba_share,microclimate_ba_share,diff_ba_share)) 


###
# 5. Sensitivity analysis
###
  
### load pre-processed iland outputs
sens.in <- list.files(path="processed_data/sensitivity_analysis/", full.names=TRUE) %>%
  map(~read_csv(.,col_types=c(output="c"))) %>%
  list_rbind()

### prep values by rep: cumNEP at year 30, all others averaged
sens.cumNEP <- sens.in %>%
  filter(year==30) %>%
  dplyr::select(c(output,barkbeetle,decomposition,establishment,rep,cumNEP))

sens.carbon <- sens.in %>%
  dplyr::select(c(output,barkbeetle,decomposition,establishment,rep,total_c_kg:soil_c_kg)) %>%
  group_by(output,barkbeetle,decomposition,establishment,rep) %>%
  summarise(across(c(total_c_kg:soil_c_kg),mean))

sens.full <- sens.cumNEP %>%
  left_join(sens.carbon, by=c("output","barkbeetle","decomposition","establishment","rep")) %>%
  # add range across all scenarios (based on mean values across reps)
  pivot_longer(c(cumNEP:soil_c_kg)) %>%
  group_by(output,name) %>%
  # mean across reps
  mutate(value_mean = mean(value)) %>%
  ungroup() %>%
  # range based on max - min for scenario means
  group_by(name) %>%
  mutate(value_max = max(value_mean),value_min=min(value_mean))

### calculate difference from macroclimate 
# macro runs only
sens.macro <- sens.full %>%
  filter(output=="000") %>%
  rename(macro=value) %>%
  dplyr::select(c(rep,name,macro))

# normalized difference, by rep
# mod formula is (micro scenario - macro baseline)/(range of scenario means)
sens.rep <- sens.full %>%
  filter(!output %in% c("000")) %>%
  left_join(sens.macro, by=c("rep","name")) %>%
  mutate(diff = value-macro) %>%
  mutate(normDiff=diff/(value_max-value_min)) %>%
  # prep for saving
  dplyr::select(-c(value:diff)) %>%
  mutate(output=paste0("scen_",output)) %>%
  pivot_wider(names_from="name",values_from="normDiff") %>%
  rename(totalC = total_c_kg,
         liveC = live_c_kg,
         deadWoodyC = deadWoody_c_kg,
         litterC = litter_c_kg,
         soilC = soil_c_kg)

# summary across reps
sens.summary <- sens.rep %>%
  group_by(output,barkbeetle,decomposition,establishment) %>%
  summarise(across(c(cumNEP:soilC),list(mean=mean,se=std.error))) 

# write out
write.csv(sens.rep,"analysis/sensitivity_analysis/sensitivity_analysis_allReps.csv",row.names=FALSE)
write.csv(sens.summary,"analysis/sensitivity_analysis/sensitivity_analysis_summary.csv",row.names=FALSE)

