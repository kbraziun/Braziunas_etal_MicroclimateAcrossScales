#####
#
## Preprocess iland outputs for sensitivity analysis
#
#####

### load libraries
library(tidyverse)
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
#   [1] RSQLite_2.3.3   lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
# [6] purrr_1.0.2     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.4  
# [11] tidyverse_2.0.0

###
# 1. arguments
###

# command line arguments
args = commandArgs(TRUE)
output = as.character(args[1])
rep = as.integer(args[2])

# # trial runs
# output = "111"
# rep = 1

###
# 1. Load and pre-process outputs
###

### load lookup table
scen.in <- read_csv("iland/sensitivity_analysis.csv") 

### load iland outputs
conn=DBI::dbConnect(RSQLite::SQLite(), dbname = paste0("iland/output/sensitivity_analysis_microclimate/output_microclimate_",rep,"_",output,".sqlite")) # connect to the db
dbListTables(conn)

# beetles
beetles.in <- tbl(conn,"barkbeetle") %>%
  dplyr::select(c(year,killedVolume)) %>%
  mutate(agent="beetles",
         output=output,
         rep=rep) %>%
  collect()

# wind
wind.in <- tbl(conn,"wind") %>%
  dplyr::select(c(year,killedVolume)) %>%
  mutate(agent="wind",
         output=output,
         rep=rep) %>%
  collect()

# carbon for respiration, current year for flow
nep.in <- tbl(conn, "carbonflow") %>% 
  dplyr::select(c(year,rid,NEP,cumNEP)) %>%
  mutate(output=output,
         rep=rep) %>%
  collect()

# total c and c pools
cpool.in <- tbl(conn, "carbon") %>% 
  filter(rid==-1) %>%
  # add all pools, no understory C, also sum live woody, dead woody, litter, soil pools
  mutate(live_c_kg = stem_c + branch_c + foliage_c + coarseRoot_c + fineRoot_c + regeneration_c,
         deadWoody_c_kg = snags_c + snagsOther_c + downedWood_c,
         litter_c_kg = litter_c,
         soil_c_kg = soil_c,
         total_c_kg = live_c_kg + deadWoody_c_kg + litter_c_kg + soil_c_kg) %>% 
  dplyr::select(c(year,total_c_kg,live_c_kg,deadWoody_c_kg,litter_c_kg,soil_c_kg)) %>%
  mutate(output=output,
         rep=rep) %>%
  collect()

dbDisconnect(conn) # close the connection

### compile outputs
out.comp <- beetles.in %>%
  rbind(wind.in) %>%
  pivot_wider(names_from="agent",values_from="killedVolume",names_prefix="killedVolume_") %>%
  mutate(across(c(killedVolume_beetles,killedVolume_wind),~replace_na(.,0))) %>%
  # add scen info
  left_join(scen.in, by="output") %>%
  # add nep
  left_join(nep.in, by=c("year","output","rep")) %>%
  left_join(cpool.in, by=c("year","output","rep")) %>%
  dplyr::select(c(year,output,barkbeetle,decomposition,establishment,rep,NEP,cumNEP,killedVolume_beetles,killedVolume_wind,total_c_kg,live_c_kg,deadWoody_c_kg,litter_c_kg,soil_c_kg))

# write out
write.csv(out.comp, paste0("processed_data/sensitivity_analysis/sensitivity_analysis_",output,"_rep_",rep,".csv"),row.names=FALSE)
