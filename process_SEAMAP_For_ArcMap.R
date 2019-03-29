# PROCESS_SEAMAP_FOR_ARCMAP ---------

# PURPOSE: clean-up output from Fish_Biomass_Step3 (Ruzicka SEAMAP query chain output) in prep for upload into ArcCatalog & ArcMap software

# DATE CREATED: 28 March 2019

# AUTHOR: Kelly L. Robinson

library(plyr)
library(dplyr)
library(data.table)
library(xlsx)

d <- as.data.frame(readxl::read_excel("FishBiomass_step3_Jellyfish.xlsx",col_names = T))

d$AggGrp_20130430 <- ifelse(test = is.na(d$AggGrp_20130430), "large_jellyfish", d$AggGrp_20130430)

d <- d[,c("AggGrp_20130430","Year","Season", "Month", "Day","STATIONID", "use_PopulationDensity_(No/m2)","use_BiomassDensity_(kg/m2)","use_Depth_(m)", "DECSLAT","DECSLON","DECELAT","DECELON"),]

d <- plyr::rename(d, c("use_BiomassDensity_(kg/m2)" = "biomass_den_kg.m2"))
d <- plyr::rename(d, c("use_PopulationDensity_(No/m2)" = "pop_den_no.m2"))
d <- plyr::rename(d, c("use_Depth_(m)" = "depth_m"))

d$biomass_den_kg.m3 <- d$biomass_den_kg.m2*d$depth_m
d$pop_den_no.m3 <- d$pop_den_no.m2*d$depth_m

d <- d %>% rowwise() %>% mutate(DECLAT_ctr = mean(DECSLAT, DECELAT, na.rm = T), DECLON_ctr = mean(DECSLON, DECELON, na.rm = T))

#subset data for ArcMAp
a <- d[,c(1:6,9,14:17)]

write.table(a, file = "jellyfish_test_arc.txt", sep =  "\t", col.names = T, row.names = F)
