setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/Indicator_Dir")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("vegan")
install.packages("ggplot2")
install.packages("reshape2")

library(readr)
#library(plyr) # n.b., some functions (e.g., rename) operate diff with plyr & dplyr
library(dplyr)
library(tidyr)
library(vegan)
#library(ggplot2)
library(reshape2)


nps_stats <- read.csv("Inputs/NPS_Parks_Name_Code_Shapefile.csv") %>%
  dplyr::rename(code = UNIT_CODE) %>%
  dplyr::rename(name = UNIT_NAME) %>%
  dplyr::rename(state = STATE) %>%
  dplyr::rename(region = REGION) %>%
  dplyr::rename(type = UNIT_TYPE) %>%
  dplyr::rename(area_km2 = Area_km2) %>%
  select(code, name, state, region, type, area_km2) %>%
  arrange(code)


################
# AREA REASSIGN#
################

# long story short, there are parks & preserves which share code.
# this includes orig nps stats shapefile (had 415 obs but 407 unique codes).
# for now, combining park & preserve area to maintain 1 code
# re-assign area by adding park & preserves for those 8 dupes
nps_area_temp <- nps_stats %>% group_by(code) %>%
  summarise_at(vars(area_km2), sum)
#remove old area
nps_stats <- nps_stats[, c("code" , "name", "state", "region", "type")]
# using join_all to ensure that only 1 value gets picked up for dupes
# otherwise, left_join gives NA for parks and preserves
nps_stats <- plyr::join(nps_area_temp, nps_stats,
                        by = "code" , type = "left",
                        match = "first")
remove(nps_area_temp)


###############
#LOAD SPP DATA#
###############

# nps_spp <- read.csv("Inputs/NPSpecies/NPSpecies_FullSpeciesListforAllParks_06142017.csv") %>%
#   select(ParkSpeciesID, ParkCode,
#          Category, ScientificName, Occurrence, Abundance,
#          Nativeness, NativenessTags, TESS, Ozone, GRank, SRank) %>%
#   arrange(ParkCode) %>%
#   write.csv(file = "Inputs/NPSpecies/nps_spp_slim.csv")
nps_spp <- read.csv("Inputs/NPSpecies/nps_spp_slim.csv")


## sprich inludes amph, bird, mamm, rept, vasc
## sprich includes [blank], present, probably present, unconfirmed.
## sprich does NOT include not-in-park.
## see NPSpecies_Explore_JULIAS.r line 122 for SpRich_vert_Vasc_ByPark.csv input
## N.b., "vv" means vertebrate & vascular
sprich <- read.csv("Inputs/NPSpecies/SpRich_Vert_Vasc_ByPark.csv") %>%
  dplyr::rename(code = ParkCode) %>%
  dplyr::rename(amph = Amphibians) %>%
  dplyr::rename(bird = Birds) %>%
  dplyr::rename(mamm = Mammals) %>%
  dplyr::rename(rept = Reptiles) %>%
  dplyr::rename(vasc = Vascular.Plants) %>%
  mutate(ttl_vv = amph + bird + mamm + rept + vasc) %>% #transmute would keep only ttl_sp
  select(code, vasc, ttl_vv) %>%
  arrange(code)


## sprich_g1g2 includes only amph, bird, mamm, rept, vasc that are g1g2
## sprich_g1g2 includes [blank], present, probably present, unconfirmed
sprich_g1g2 <- read.csv("Inputs/NPSpecies/SpRich_Vert_Vasc_G1G2_ByPark.csv") %>%
  dplyr::rename(code = ParkCode) %>%
  dplyr::rename(amph_g1g2 = TRUE_Amphibians) %>%
  dplyr::rename(bird_g1g2 = TRUE_Birds) %>%
  dplyr::rename(mamm_g1g2 = TRUE_Mammals) %>%
  dplyr::rename(rept_g1g2 = TRUE_Reptiles) %>%
  dplyr::rename(vasc_g1g2 = TRUE_Vascular.Plants) %>%
  mutate(ttl_vv_g1g2 = amph_g1g2 + bird_g1g2 + mamm_g1g2 + rept_g1g2 + vasc_g1g2) %>%
  select(code, ttl_vv_g1g2) %>%
  arrange(code)


## sprich_nat includes amph, bird, mamm, rept, vasc divided by native and non-native
## sprich_nat includes [blank], present, probably present, unconfirmed
## see NPSpecies_Explore_JULIAS.r line 172 for SpRich_Vert_Vasc_Nativeness_ByPark.csv input 
sprich_nat <- read.csv("Inputs/NPSpecies/SpRich_Vert_Vasc_Nativeness_ByPark.csv") %>%
  dplyr::rename(code = ParkCode) %>%
  dplyr::rename(amph_nat = Native_Amphibians) %>%
  dplyr::rename(bird_nat = Native_Birds) %>%
  dplyr::rename(mamm_nat = Native_Mammals) %>%
  dplyr::rename(rept_nat = Native_Reptiles) %>%
  dplyr::rename(vasc_nat = Native_Vascular.Plants) %>%
  #
  dplyr::rename(amph_nonnat = Non.native_Amphibians) %>%
  dplyr::rename(bird_nonnat = Non.native_Birds) %>%
  dplyr::rename(mamm_nonnat = Non.native_Mammals) %>%
  dplyr::rename(rept_nonnat = Non.native_Reptiles) %>%
  dplyr::rename(vasc_nonnat = Non.native_Vascular.Plants) %>%
  #
  mutate(ttl_vv_nat = amph_nat + bird_nat + mamm_nat + rept_nat + vasc_nat) %>%
  mutate(ttl_vv_nonnat = amph_nonnat + bird_nonnat + mamm_nonnat + rept_nonnat + vasc_nonnat) %>%
  #
  select(code, ttl_vv_nat, ttl_vv_nonnat, vasc_nat, vasc_nonnat) %>%
  arrange(code)
  #
  
## sprich_vasc_o3 includes ozone sensitive vasc spp, biological indicators
sprich_vasc_o3 <- read.csv("Inputs/NPSpecies/SpRich_Vasc_Ozone_ByPark.csv") %>%
  dplyr::rename(code = ParkCode) %>%
  dplyr::rename(vasc_o3 = Oz_Tot) %>% # note sp either o or o+bi, hence ttl
  select(code, vasc_o3) %>%
  arrange(code)



## FISHIES
fish_rich <- read.csv("Inputs/Fish Species/Lawrence_NPS_fish_richness.csv") %>%
  select(UNIT_CODE, Fish.species.richness)
fish_g1g2 <- read.csv("Inputs/Fish Species/Lawrence_NPS_fish_G1G2.csv") %>%
  select(UNIT_CODE, Fish_G1G2)
fish_nonnat <- read.csv("Inputs/Fish Species/Lawrence_NPS_fish_nonnative.csv") %>%
  select(UNIT_CODE, Non.native.richness)

fish <- fish_rich %>%
  plyr::join(fish_nonnat, by = "UNIT_CODE", type = "left") %>%
  plyr::join(fish_g1g2, by = "UNIT_CODE", type = "left") %>%
  dplyr::rename(code = UNIT_CODE) %>%
  dplyr::rename(fish = Fish.species.richness) %>%
  dplyr::rename(fish_g1g2 = Fish_G1G2) %>%
  dplyr::rename(fish_nonnat = Non.native.richness) %>%
  select(code, fish, fish_g1g2, fish_nonnat)

remove(fish_g1g2, fish_nonnat, fish_rich)


#########
#SUMMARY#
#########

# using new nps stats. not including sprich -- just g1g2, native, ozone
library(plyr)
spp_sum <- join_all(list(nps_stats,
                         sprich,
                         sprich_g1g2,
                         sprich_nat,
                         sprich_vasc_o3,
                         fish),
                 by='code',
                 type = 'left',
                 match = 'first')

remove(sprich, sprich_g1g2, sprich_nat, sprich_vasc_o3)

currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_raw_",currentDate,".csv",sep="")
write.csv(spp_sum, file = csvFileName)



spp_sum_FINAL <- spp_sum
# g1g2
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(ttl_vv_g1g2_prop = ttl_vv_g1g2/ttl_vv)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(ttl_vv_g1g2_AW = ttl_vv_g1g2/area_km2)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(fish_g1g2_prop = fish_g1g2/fish)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(fish_g1g2_AW = fish_g1g2/area_km2)
# ozone-sensitive
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(vasc_o3_prop = vasc_o3/vasc)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(vasc_o3_AW = vasc_o3/area_km2)
# nonnative
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(ttl_vv_nonnat_prop = (ttl_vv_nonnat/ttl_vv))
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(ttl_vv_nonnat_AW = (ttl_vv_nonnat/area_km2))
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(vasc_nonnat_prop = vasc_nonnat/vasc)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(vasc_nonnat_AW = vasc_nonnat/area_km2)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(fish_nonnat_prop = fish_nonnat/fish)
spp_sum_FINAL <- spp_sum_FINAL %>% mutate(fish_nonnat_AW = fish_nonnat/area_km2)


spp_sum_FINAL <- spp_sum_FINAL %>% select(code, area_km2, name, state, region, type,
                                         ttl_vv_g1g2, ttl_vv_g1g2_prop, ttl_vv_g1g2_AW,
                                         fish_g1g2, fish_g1g2_prop, fish_g1g2_AW,
                                         vasc_o3, vasc_o3_prop, vasc_o3_AW,
                                         ttl_vv_nonnat, ttl_vv_nonnat_prop, ttl_vv_nonnat_AW,
                                         vasc_nonnat, vasc_nonnat_prop, vasc_nonnat_AW,
                                         fish_nonnat, fish_nonnat_prop, fish_nonnat_AW)

currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_FINAL_",currentDate,".csv",sep="")
write.csv(spp_sum_FINAL, file = csvFileName)

# Also write to upper-level Input folder for use in big, non-sp summary table
currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_FINAL_",currentDate,".csv",sep="")
write.csv(spp_sum_FINAL, file = csvFileName)
         
##############
#CORRELATIONS#
##############

# correlations only work on numeric; drop text columns
drops <- c("code", "name", "state", "region", "type")
temp <- spp_sum_FINAL[, !(names(spp_sum_FINAL) %in% drops)]
# generate correlation matrix for all spp indicators
spp_sum_FINAL_corr <- round(cor(temp, method = "spearman",
                                use = "na.or.complete"), 2)
# write correlation matrix to csv & drop temp files
currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_FINAL_corr_",currentDate,".csv",sep="")
write.csv(spp_sum_FINAL_corr, file = csvFileName)               
remove(drops, temp)

############
#NICE NAMES#
############

# convert short-hand to nice names
spp_sum_FINAL_nicenames <- spp_sum_FINAL %>%
  dplyr::rename("Unit code" = code,
                "Unit name" = name,
                "Area (km^2)" = area_km2,
                State = state,
                Region = region,
                "Unit type" = type,
                "Ttl g1g2 terr vert & vasc spp" = ttl_vv_g1g2,
                "Prop g1g2 terr vert & vasc spp" = ttl_vv_g1g2_prop,
                "Ttl g1g2 terr vert & vasc spp - area-wt" = ttl_vv_g1g2_AW,
                "Ttl fish spp" = fish_g1g2,
                "Prop g1g2 fish spp" = fish_g1g2_prop,
                "Ttl fish spp - area-wt" = fish_g1g2_AW,
                "Ozone-sens vasc spp" = vasc_o3,
                "Prop ozone-sens vasc spp" = vasc_o3_prop,
                "Ozone-sens vasc spp - area-wt" = vasc_o3_AW,
                "Non-native terr vert & vasc spp" = ttl_vv_nonnat,
                "Prop nonnative terr vert & vasc spp" = ttl_vv_nonnat_prop,
                "Non-native terr vert & vasc spp - area-wt" = ttl_vv_nonnat_AW,
                "Non-native vasc spp" = vasc_nonnat,
                "Prop nonnative vasc spp" = vasc_nonnat_prop,
                "Non-native vasc spp - area-wt" = vasc_nonnat_AW,
                "Non-native fish spp" = fish_nonnat,
                "Prop nonnative fish spp" = fish_nonnat_prop,
                "Non-native fish spp - area-wt" = fish_nonnat_AW,
                "Prop nonnative vasc spp" = vasc_nonnat_prop,
                "Prop nonnative fish spp" = fish_nonnat_prop)
                
# Write nice name csv                
currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_FINAL_nicenames_",currentDate,".csv",sep="")
write.csv(spp_sum_FINAL_nicenames, file = csvFileName) 


# correlations only work on numeric; drop text columns
drops <- c("Unit code", "Unit name", "State", "Region", "Unit type")
temp <- spp_sum_FINAL_nicenames[, !(names(spp_sum_FINAL_nicenames) %in% drops)]
# generate correlation matrix for all spp indicators
spp_sum_FINAL_corr_nicenames <- round(cor(temp, method = "spearman",
                                use = "na.or.complete"), 2)
remove(drops, temp)

# Write nice name correlation csv
currentDate <- Sys.Date()
csvFileName <- paste("Outputs/spp_sum_FINAL_corr_nicenames_",currentDate,".csv",sep="")
write.csv(spp_sum_FINAL_corr_nicenames, file = csvFileName) 

remove(csvFileName)
remove(currentDate)
