#Code for SEAMAP database from Access queries by Jim Ruzicka
#Stacy Calhoun, 31 Aug 18

remove(list=ls())
assign("last.warning", NULL, envir = baseenv())

# Libraries and Datasets --------------------------------------------------

library(plyr)
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)


#functions -----
clean.quotes <- function(y){
    gsub(pattern = '"', replacement = "", x = y)
  }
clean.whitespace<- function(y){
  gsub(pattern = ' ', replacement = "", x = y)
}


# Set specific interest groups from Name Translator Table ----
NameTrans = read_xlsx(path = 'NameTranslator_table201305.xlsx')
#View(NameTrans)

major.grp = "jellyfish"
taxa = c("CHRYSAORA","CHRYSAORA QUINQUECIRRHA","DACTYLOMETRA QUINQUECIRRHA")

# load SEAMAP data files "https://seamap.gsmfc.org/datarequests/index.php" -----
INGEST_DATA = read.csv('20190329_SEAMAP_csv/INGEST_DATA.csv')
INVREC = read.csv('20190329_SEAMAP_csv/INVREC.csv')
ISTREC = read.csv('20190329_SEAMAP_csv/ISTREC.csv')
NEWBIOCODESBIG = read.csv('20190329_SEAMAP_csv/NEWBIOCODESBIG.csv')
SHRREC = read.csv('20190329_SEAMAP_csv/SHRREC.csv')
VESSELS = read.csv('20190329_SEAMAP_csv/VESSELS.csv')
CTDCASTREC = read.csv('20190329_SEAMAP_csv/CTDCASTREC.csv')
CTDREC = read.csv('20190329_SEAMAP_csv/CTDREC.csv')
GLFREC = read.csv('20190329_SEAMAP_csv/GLFREC.csv')
BGSREC = read.csv('20190329_SEAMAP_csv/BGSREC.csv')
ENVREC = read.csv('20190329_SEAMAP_csv/ENVREC.csv')
STAREC = read.csv(file = "20190329_SEAMAP_csv/STAREC_rev20190402.csv", stringsAsFactors = F, header = T, quote = "", fill = T) #updated to read-in all the lines of the text file
CRUISES = read.csv('20190329_SEAMAP_csv/CRUISES.csv')


#cleanup for STAREC ---------
# get field names
s.names = suppressWarnings(read.csv(file = "20190329_SEAMAP_csv/STAREC_rev20190402.csv", stringsAsFactors = F, header = T))
names(STAREC) <- names(s.names)
rm(s.names)

#clean quotes off of data
STAREC[] <- sapply(X = STAREC, FUN = clean.quotes)
STAREC$GEARS <- sapply(X = STAREC$GEARS, FUN = clean.whitespace)


#START QUERY CHAIN --------


#Fish_Count_Step_1 ------------------------------------------------------------
FC1 = subset(BGSREC, select = c("BIO_BGS", "CRUISEID", "STATIONID", "VESSEL", "CRUISE_NO",
                                "CNT", "CNTEXP", "SAMPLE_BGS", "SELECT_BGS"))
NT = subset(NameTrans, select = c("NODC_code", "TAXONOMIC", "major_group", "common_name",
           "AggGrp_20130430", "Habitat"))

FC1_NT = merge(FC1, NT, by.x = "BIO_BGS", by.y = "NODC_code", all.x = T)
colnames(FC1_NT)[1] = "NODC_code"

#major_group can be set as needed as well as the aggregate group.
#Fish_Cnt_1 = subset(FC1_NT, major_group == 'jellyfish' & TAXONOMIC == "AURELIA" | TAXONOMIC == "AURELIA AURITA") #Depecrated to allow for selection of groups at the top of the script

Fish_Cnt_1 = subset(FC1_NT, major_group %in% major.grp & TAXONOMIC %in% taxa)


# Fish_Count_Step_2 ------------------------------------------------------------


Fish_Cnt_2 = ddply(Fish_Cnt_1, c("NODC_code", "TAXONOMIC", "common_name", "AggGrp_20130430", "Habitat",
                                 "CRUISEID", "STATIONID", "VESSEL", "CRUISE_NO"), summarize,
                   SumofCNT = sum(CNT),
                   SumofCNTE = sum(CNTEXP),
                   SumofSample_BGS = sum(SAMPLE_BGS),
                   SumofSelect_BGS = sum(SELECT_BGS),
                   CountofCNT = length(CNT),
                   .progress = "text", .inform = T )



# Fish_Count_Step_3 ------------------------------------------------------------
#Renaming Columns
Fish_Cnt_3 = setnames(Fish_Cnt_2, old = c('SumofCNT', 'SumofCNTE', 'SumofSample_BGS', 'SumofSelect_BGS', 'CountofCNT'),
         new = c('Tot_Sample_Count', 'Tot_Extrap_Count', 'Tot_Sample_WWT_kg', 'Tot_Extrap_WWT_kg', 'Stations_Added'))

Fish_Cnt_3$all_counted[Fish_Cnt_3$Tot_Sample_Count == Fish_Cnt_3$Tot_Extrap_Count] = 'yes'
Fish_Cnt_3$all_counted[Fish_Cnt_3$Tot_Sample_Count != Fish_Cnt_3$Tot_Extrap_Count] = 'no'

Fish_Cnt_3$Mean_Extrap_WWT_g = with(Fish_Cnt_3,
                                    ifelse(all_counted == 'yes', (Tot_Extrap_WWT_kg*1000)/Tot_Extrap_Count,
                                                       (Tot_Sample_WWT_kg*1000)/Tot_Sample_Count))


# Fish_Count_Step_4 -------------------------------------------------------

Fish_Cnt_4 = ddply(Fish_Cnt_3, c("NODC_code", "TAXONOMIC", "common_name", "AggGrp_20130430", "Habitat",
                                 "CRUISEID", "VESSEL", "CRUISE_NO"), summarize,
                   AvgofMean = mean(Mean_Extrap_WWT_g),
                   CountofMean = length(Mean_Extrap_WWT_g),
                   .progress = "text", .inform = T)

# Fish_Count_Step_5 -------------------------------------------------------

Fish_Cnt_5 = merge(Fish_Cnt_3, Fish_Cnt_4)

setnames(Fish_Cnt_5, old = 'AvgofMean', new = 'CruiseMean_Extrap_WWT_g')

Fish_Cnt_5[, c('Tot_Sample_WWT_kg', 'Tot_Sample_Count', 'Stations_Added', 'CountofMean')] = list(NULL)


# Fish_Weight_Step_1 ------------------------------------------------------

FW1 = subset(GLFREC, select = c("BIO_GLF", "LEN_GLF", "MEASCD_GLF", "CRUISEID", "STATIONID", "VESSEL", "CRUISE_NO"))

NT2 = subset(NameTrans, select = c("NODC_code", "TAXONOMIC", "common_name", "major_group", "AggGrp_20130430",
                                   "FL_2_TL_a", "FL_2_TL_b", "SL_2_TL_a", "SL_2_TL_b", "TL_2_SL_a", "TL_2_SL_b",
                                   "TL_2_FL_a", "TL_2_FL_b", "LW_a", "LW_b", "LW_type", "mean_WWT_(g)"))
setnames(NT2, old = "mean_WWT_(g)", new = "mean_WWT_g")
NT2$mean_WWT_g = as.numeric(NT2$mean_WWT_g)

Fish_Wt_1 = merge(NT2, FW1, by.x ="NODC_code" , by.y = "BIO_GLF")

#Fish_Wt_1 = subset(Fish_Wt_1, major_group == 'jellyfish' & TAXONOMIC == "AURELIA" | TAXONOMIC == "AURELIA AURITA")
Fish_Wt_1 = subset(Fish_Wt_1, major_group %in% major.grp & TAXONOMIC %in% taxa)


#Create Measurement type column
Fish_Wt_1$Measurement_Type = with(Fish_Wt_1, ifelse(MEASCD_GLF == 18, "TL", ifelse(MEASCD_GLF == 2, "SL", "FL")))


#Creating use_TL_cm column
Fish_Wt_1 <- ddply(Fish_Wt_1, .(Measurement_Type), function(x){

  m_type <- unique(x$Measurement_Type)

  if(m_type =="TL"){
    x$use_TL_cm <- x$LEN_GLF/10

  } else if(m_type =="SL"){
    x$use_TL_cm <- (x$SL_2_TL_a + x$SL_2_TL_b * (x$LEN_GLF)/10)

  } else if(m_type =="FL"){
    x$use_TL_cm <- (x$FL_2_TL_a + x$FL_2_TL_b * (x$LEN_GLF)/10)

  } else {
    x$use_TL_cm <- NA
  }

  return(x)

}, .progress = "text", .inform = T)


#Creating use_SL_cm and use_FL_cm columns
Fish_Wt_1$use_SL_cm = Fish_Wt_1$TL_2_SL_a + Fish_Wt_1$TL_2_SL_b * Fish_Wt_1$use_TL_cm
Fish_Wt_1$use_FL_cm = Fish_Wt_1$TL_2_FL_a + Fish_Wt_1$TL_2_FL_b * Fish_Wt_1$use_TL_cm

#Creating WWT_g column

Fish_Wt_1 = ddply(Fish_Wt_1, .(LW_type), function(y) {

  LW = unique(y$LW_type)

  LW <- ifelse(is.na(LW),"NOPE",LW) #address 'NA' values for jellyfish and other non-fish/inverts in Name Translator

  if(LW =="TL"){
    y$WWT_g <- y$LW_a * y$use_TL_cm^y$LW_b

  } else if (LW =="FL"){
    y$WWT_g <- y$LW_a * y$use_FL_cm^y$LW_b

  } else if (LW =="SL"){
    y$WWT_g <- y$LW_a * y$use_SL_cm^y$LW_b

  } else {
    taxa <- unique(Fish_Wt_1$TAXONOMIC)
    y$WWT_g <- unique(NT2[NT2$TAXONOMIC %in% taxa,]$mean_WWT_g)

  }
  return(y)
}, .progress = "text", .inform = T)

# Fish_Weight_Step_2 ------------------------------------------------------

Fish_Wt_2 = ddply(Fish_Wt_1, c("NODC_code", "TAXONOMIC", "common_name", "AggGrp_20130430", "CRUISEID",
                               "STATIONID", "VESSEL", "CRUISE_NO", "mean_WWT_g"), summarize,
                  CountofWWT_g = length(WWT_g),
                  SumofWWT_g = sum(WWT_g),
                .progress = "text", .inform = T)

setnames(Fish_Wt_2, old = "mean_WWT_g", new = "ForcedMean_WWT_g")

# Fish_Weight_Step_3 ------------------------------------------------------

Fish_Wt_3 = setnames(Fish_Wt_2, old = c("SumofWWT_g", "CountofWWT_g"),
                     new = c("Total_WWT_g", "number_measured"))
Fish_Wt_3$mean_WWT_g = Fish_Wt_3$Total_WWT_g/Fish_Wt_3$number_measured


# Fish_Weight_Step_4 ------------------------------------------------------

Fish_Wt_4 = ddply(Fish_Wt_3, c("NODC_code", "TAXONOMIC", "common_name", "AggGrp_20130430", "CRUISEID",
                               "VESSEL", "CRUISE_NO"), summarize,
                  Avg_mean_WWT_g = mean(mean_WWT_g),
                  Count_mean_WWT_g = length(mean_WWT_g),
                  .progress = "text", .inform = T )

# Fish_Weight_Step_5 ------------------------------------------------------

FW4 = subset(Fish_Wt_4, select = c("NODC_code","Avg_mean_WWT_g"))

FW4 <- FW4 %>% distinct(NODC_code, .keep_all = TRUE) #remove any duplicated average weights for each NODC code

Fish_Wt_5 = merge(x = FW4, y = Fish_Wt_3, by = "NODC_code", all.y = T)

setnames(Fish_Wt_5, old = "Avg_mean_WWT_g", new = "CruiseMean_WWT_g")


# StationInfo_Step_1 ------------------------------------------------------
STA = subset(STAREC, select = c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO", "DECSLAT", "DECSLON", "DECELAT",
                                "DECELON", "MO_DAY_YR", "DEPTH_SSTA", "DEPTH_ESTA", "VESSEL_SPD"))
#fix data types from STA
numeric.cols <- c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO", "DECSLAT", "DECSLON", "DECELAT",
                    "DECELON", "DEPTH_SSTA", "DEPTH_ESTA", "VESSEL_SPD")

STA[,numeric.cols] = suppressWarnings(apply(STA[,numeric.cols], 2, function(x) as.numeric((x)))) #make numeric columns actually numeric data type
STA$MO_DAY_YR <- as.POSIXct(strptime(x = STA$MO_DAY_YR, format = "%Y-%m-%d")) #change data type for date field
STA <- STA[rowSums(is.na(STA)) != ncol(STA), ] #Remove records where ALL fields have NA values

ENV = subset(ENVREC, select = c("STATIONID", "DEPTH_EWTR", "DEPTH_EMAX"))

#STA$MO_DAY_YR = as.Date(STA$MO_DAY_YR, "%m/%d/%y")

StationInfo_1 = merge(STA, ENV, by = "STATIONID")


#Create DECSLAT and DECSLON_radians columns

StationInfo_1$DECSLAT_radians = StationInfo_1$DECSLAT*(3.1459/180)
StationInfo_1$DECSLON_radians = StationInfo_1$DECSLON*(3.1459/180)


#Create DECELAT and DECELON_radians columns

StationInfo_1$DECELAT_radians = with(StationInfo_1, ifelse(is.na(DECELAT) | DECELAT == 0, NA,
                                                           DECELAT*(3.14159/180)))

StationInfo_1$DECELON_radians = with(StationInfo_1, ifelse(is.na(DECELON) | DECELON == 0, NA,
                                                           DECELON*(3.14159/180)))

#Create Spherical Law column

StationInfo_1$SphericalLaw_X = sin(StationInfo_1$DECSLAT_radians) * sin(StationInfo_1$DECELAT_radians) +
  cos(StationInfo_1$DECSLAT_radians) * cos(StationInfo_1$DECELAT_radians) *
  cos((StationInfo_1$DECELON_radians - StationInfo_1$DECSLON_radians))

#Create subregion column

StationInfo_1$Subregion_alongshore = with(StationInfo_1, ifelse(DECSLON < -94, "1_Tex",
                                                                ifelse(DECSLON > -88, "3_Fla", "2_Lou")))

#Create Year, Month, Day, and Season columns

StationInfo_1$Year = year(StationInfo_1$MO_DAY_YR)
StationInfo_1$Month = month(StationInfo_1$MO_DAY_YR)
StationInfo_1$Day = day(StationInfo_1$MO_DAY_YR)

StationInfo_1$Season = with(StationInfo_1, ifelse(Month >= 5 & Month <= 8, 'Summer', 'Fall'))

#Create Depth columns

StationInfo_1$Depth_SSTA_m = StationInfo_1$DEPTH_SSTA*1.8288
StationInfo_1$Depth_ESTA_m = StationInfo_1$DEPTH_ESTA*1.8288


StationInfo_1$DEPTHEMAXvsEWTR_m = with(StationInfo_1,
                                       ifelse(DEPTH_EWTR >= DEPTH_EMAX | is.na(DEPTH_EMAX), DEPTH_EWTR, DEPTH_EMAX))

StationInfo_1$DEPTH_SSTAvsESTA_m = with(StationInfo_1,
                                        ifelse(DEPTH_ESTA >= DEPTH_SSTA | is.na(DEPTH_SSTA), DEPTH_ESTA, DEPTH_SSTA))


StationInfo_1$Use_Depth_m = with(StationInfo_1,
                                 ifelse(DEPTH_SSTAvsESTA_m >= DEPTHEMAXvsEWTR_m | is.na(DEPTHEMAXvsEWTR_m),
                                        DEPTH_SSTAvsESTA_m, DEPTHEMAXvsEWTR_m))


# add some depths back in using spatial join from ArcMAp
# Data from ArcMap Spatial Join
j <- fread("jelly_inshore_20m_NULL_depth_4km_join.txt", stringsAsFactors = F, header = T)
ji <- j[,c("stationid", "depth_m_12","year", "month", "day")]
ji <- plyr::rename(ji, c("depth_m_12" = "Use_Depth_m_Arc"))
ji <- plyr::rename(ji, c("stationid" = "STATIONID"))
ji <- plyr::rename(ji, c("year" = "Year"))
ji <- plyr::rename(ji, c("month" = "Month"))
ji <- plyr::rename(ji, c("day" = "Day"))

j <- fread("jelly_shelf_200m_NULL_depth_4km_join.txt", stringsAsFactors = F, header = T)
js <- j[,c("stationid", "depth_m_12","year", "month", "day")]
js <- plyr::rename(js, c("depth_m_12" = "Use_Depth_m_Arc"))
js <- plyr::rename(js, c("stationid" = "STATIONID"))
js <- plyr::rename(js, c("year" = "Year"))
js <- plyr::rename(js, c("month" = "Month"))
js <- plyr::rename(js, c("day" = "Day"))

ja <- rbind(ji,js)

StationInfo_1 <- merge(x = StationInfo_1, y = ja, by = c("STATIONID", "Year", "Month", "Day"), all.x = T)

StationInfo_1$Use_Depth_m = with(StationInfo_1,
                                     ifelse(test = is.na(Use_Depth_m), yes = ifelse(is.na(Use_Depth_m_Arc),
                                                                                    yes = NA, no = Use_Depth_m_Arc), no = Use_Depth_m))
StationInfo_1$Use_Depth_m_Arc <- NULL

StationInfo_1 <- StationInfo_1[StationInfo_1$Use_Depth_m > 0,] #remove any 'zero' depth stations as not legit
StationInfo_1 <- StationInfo_1[!is.na(StationInfo_1$Use_Depth_m),] #remove any 'NA' depth stations as not legit

StationInfo_1$Subregion_Depth = with(StationInfo_1,
                                     ifelse(Use_Depth_m <= 20, "1_inshore",ifelse(is.na(Use_Depth_m), NA,
                                            ifelse(Use_Depth_m <= 200, "2_shelf", "3_oceanic"))))
#Create corrected vessel speed column

StationInfo_1$VESSEL_SPD_corrected = with(StationInfo_1, ifelse(is.na(VESSEL_SPD), 3, VESSEL_SPD))


# StationInfo_Step_2 ------------------------------------------------------

StationInfo_2 = subset(StationInfo_1, select = c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO",
                                                 "Subregion_alongshore","Subregion_Depth", "Year", "Month",
                                                 "Day", "Season", "SphericalLaw_X", "VESSEL_SPD_corrected",
                                                 "Use_Depth_m", "DECSLAT", "DECSLON", "DECELAT", "DECELON"))


#Creating ArcCos, Distance Trawled, and Water column fishing time columns
StationInfo_2$ArcCos = 2 * atan(((sqrt(1-(StationInfo_2$SphericalLaw_X)^2)) / (1 + StationInfo_2$SphericalLaw_X)))


StationInfo_2$DistanceTrawledSpLawCos_m = (StationInfo_2$ArcCos*6371)*1000

StationInfo_2$WaterColumnFishingTime_sec = 2*((StationInfo_2$Use_Depth_m/(cos(60*(3.14/180))))/0.75)


# StationInfo_Step_3 -----------------------------------------------------------

IN = subset(INVREC, select = c("STATIONID", "GEAR_SIZE", "GEAR_TYPE"))

StationInfo_3 = merge(StationInfo_2, IN, by = "STATIONID")

#Discard unneeded columns
StationInfo_3[, c('SphericalLaw_X', 'ArcCos')] = list(NULL)

#Calculations to determine volume and area filtered

StationInfo_3$NetOpening_m2 = (3.14*StationInfo_3$GEAR_SIZE)*0.09

StationInfo_3$VolumeFiltered_Demersal_m3 = StationInfo_3$NetOpening_m2*StationInfo_3$DistanceTrawledSpLawCos_m

StationInfo_3$DistanceTrawled_Pelagic_m = (StationInfo_3$VESSEL_SPD_corrected * 0.51) * StationInfo_3$WaterColumnFishingTime_sec

StationInfo_3$VolumeFiltered_Pelagic_m3 = StationInfo_3$NetOpening_m2*StationInfo_3$DistanceTrawled_Pelagic_m

StationInfo_3$AreaFiltered_Pelagic_m2 = StationInfo_3$VolumeFiltered_Pelagic_m3/StationInfo_3$Use_Depth_m

StationInfo_3$AreaFiltered_Demersal_m2 = StationInfo_3$VolumeFiltered_Demersal_m3/StationInfo_3$Use_Depth_m



# Fish_Biomass_Step_1 -----------------------------------------------------

FW5 = subset(Fish_Wt_5, select = c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO", "NODC_code",
                                   "Total_WWT_g", "mean_WWT_g", "CruiseMean_WWT_g", "ForcedMean_WWT_g"))

Fish_Biomass_1 = merge(x = Fish_Cnt_5, y = FW5,
                       by = c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO", "NODC_code"),
                       all.x = T)

#Create Use_Count column
Fish_Biomass_1$Use_Count = with(Fish_Biomass_1, ifelse(is.na(Tot_Extrap_Count), 0, Tot_Extrap_Count))


#Create Weight columns

Fish_Biomass_1$Weight_A = (Fish_Biomass_1$mean_WWT_g*Fish_Biomass_1$Use_Count)/1000
Fish_Biomass_1$Weight_B = (Fish_Biomass_1$CruiseMean_WWT_g*Fish_Biomass_1$Use_Count)/1000
Fish_Biomass_1$Weight_C = (Fish_Biomass_1$Mean_Extrap_WWT_g*Fish_Biomass_1$Use_Count)/1000
Fish_Biomass_1$Weight_D = (Fish_Biomass_1$CruiseMean_Extrap_WWT_g*Fish_Biomass_1$Use_Count)/1000
Fish_Biomass_1$Weight_E = (Fish_Biomass_1$ForcedMean_WWT_g*Fish_Biomass_1$Use_Count)/1000


#Create use_Tot_WWT_kg column

wt_list <- list()

pb = txtProgressBar(min = 0, max = nrow(Fish_Biomass_1), initial = 0, style = 3)

for(i in 1:nrow(Fish_Biomass_1)){

if(!is.na(Fish_Biomass_1$Weight_A[i])){

  Use_Tot_WWT_kg <- Fish_Biomass_1$Weight_A[i]

} else if(!is.na(Fish_Biomass_1$Weight_B[i])){

  Use_Tot_WWT_kg <- Fish_Biomass_1$Weight_B[i]

} else if(!is.na(Fish_Biomass_1$Weight_C[i])){

  Use_Tot_WWT_kg <- Fish_Biomass_1$Weight_C[i]

} else if(!is.na(Fish_Biomass_1$Weight_D[i])){

  Use_Tot_WWT_kg <- Fish_Biomass_1$Weight_D[i]

} else if(!is.na(Fish_Biomass_1$Weight_E[i])){

  Use_Tot_WWT_kg <- Fish_Biomass_1$Weight_E[i]

} else {

  Use_Tot_WWT_kg <- 0

}

wt_list[[i]] <- Use_Tot_WWT_kg

setTxtProgressBar(pb,i)

}

df <- do.call(rbind, wt_list)
df <- as.data.frame(df)
colnames(df)[1] = "Use_Tot_WWT_kg"

Fish_Biomass_1 = cbind(Fish_Biomass_1, df)


# Fish_Biomass_Step_2 -----------------------------------------------------

FB2 = subset(Fish_Biomass_1, select = c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO",
                                                   "AggGrp_20130430", "Habitat", "Use_Count", "Use_Tot_WWT_kg"))

Fish_Biomass_2 = ddply(FB2, c("STATIONID", "CRUISEID", "VESSEL", "CRUISE_NO","AggGrp_20130430", "Habitat"), summarize,
                    SumofUse_Count = sum(Use_Count),
                    SumofUse_Tot_WWT_kg = sum(Use_Tot_WWT_kg),
                    Count_Use_Count = length(Use_Count),
                    .progress = "text", .inform = T)



# Fish_Biomass_Step_3 -----------------------------------------------------
#Subset and merge needed dataframes
SI3 = subset(StationInfo_3, select = c("STATIONID", "Subregion_alongshore", "Subregion_Depth", "Year", "Month", "Day",
                                       "Season", "VolumeFiltered_Demersal_m3", "VolumeFiltered_Pelagic_m3",
                                      "AreaFiltered_Demersal_m2", "AreaFiltered_Pelagic_m2"))

Fish_Biomass_3 = merge(Fish_Biomass_2, SI3, by = "STATIONID")

#Remove unneeded columns
#Fish_Biomass_3[, c("Use_Count", "Use_Tot_WWT_kg")] = list(NULL)

#Population density
Fish_Biomass_3$Pop_Den_Demersal_no_m2 = Fish_Biomass_3$SumofUse_Count/Fish_Biomass_3$AreaFiltered_Demersal_m2
Fish_Biomass_3$Pop_Den_Pelagic_no_m2 = Fish_Biomass_3$SumofUse_Count/Fish_Biomass_3$AreaFiltered_Pelagic_m2

#Biomass density
Fish_Biomass_3$Biomass_Den_Demersal_kg_m2 = Fish_Biomass_3$SumofUse_Tot_WWT_kg/Fish_Biomass_3$AreaFiltered_Demersal_m2
Fish_Biomass_3$Biomass_Den_Pelagic_kg_m2 = Fish_Biomass_3$SumofUse_Tot_WWT_kg/Fish_Biomass_3$AreaFiltered_Pelagic_m2

Fish_Biomass_3$Use_Pop_Den_no_m2 = with(Fish_Biomass_3,
                                        ifelse(Habitat == 'demersal', Pop_Den_Demersal_no_m2, Pop_Den_Pelagic_no_m2))

Fish_Biomass_3$Use_Biomass_Den_kg_m2 = with(Fish_Biomass_3,
                                            ifelse(Habitat == 'demersal', Biomass_Den_Demersal_kg_m2, Biomass_Den_Pelagic_kg_m2))

#write.csv(Fish_Biomass_3, "SEAMAP_query_data.csv")


# Adding Coordinates and Dates to Fish_Biomass_3 (including stations where target group was not collected) --------------------------

SI2 = subset(StationInfo_3, select = c("DECSLAT", "DECSLON", "DECELAT", "DECELON", "CRUISE_NO",
                                       "CRUISEID", "STATIONID", "Year", "Month", "Day",
                                       "Subregion_Depth", "Subregion_alongshore", "Use_Depth_m"))

result = merge(Fish_Biomass_3, SI2, by = c("STATIONID", "CRUISEID", "CRUISE_NO", "Year", "Month", "Day",
                                            "Subregion_Depth", "Subregion_alongshore"), all.y = T)

# remove the 3 records with infinite values due to zeros in volume filtered calculation
result <- result[!is.infinite(result$Use_Biomass_Den_kg_m2),]

#Changing NAs to zeros for population or biomass density columns for stations at which none of the target group was collected.

result$Use_Pop_Den_no_m2 = with(result,
                                 ifelse(is.na(Use_Pop_Den_no_m2), 0,Use_Pop_Den_no_m2))

result$Use_Biomass_Den_kg_m2 = with(result,
                                 ifelse(is.na(Use_Biomass_Den_kg_m2), 0,Use_Biomass_Den_kg_m2))

result$SumofUse_Count = with(result,
                                 ifelse(is.na(SumofUse_Count), 0,SumofUse_Count))

result$SumofUse_Tot_WWT_kg = with(result,
                                 ifelse(is.na(SumofUse_Tot_WWT_kg), 0,SumofUse_Tot_WWT_kg))

result$Pop_Den_Pelagic_no_m2 = with(result,
                                 ifelse(is.na(Pop_Den_Pelagic_no_m2), 0,Pop_Den_Pelagic_no_m2))

result$Pop_Den_Demersal_no_m2 = with(result,
                                 ifelse(is.na(Pop_Den_Demersal_no_m2), 0,Pop_Den_Demersal_no_m2))

result$Biomass_Den_Pelagic_kg_m2 = with(result,
                                 ifelse(is.na(Biomass_Den_Pelagic_kg_m2), 0,Biomass_Den_Pelagic_kg_m2))

result$Biomass_Den_Demersal_kg_m2 = with(result,
                                 ifelse(is.na(Biomass_Den_Demersal_kg_m2), 0,Biomass_Den_Demersal_kg_m2))

result$Season = with(result, ifelse(test = Month <= 8 & Month > 5, yes = "summer",
                                    no = ifelse(test = Month >= 9 & Month <= 11, yes = "fall",
                                                no = ifelse(test = Month >2 & Month <= 4, yes = "spring", no = "winter"))))


result$Count_Use_Count = with(result, ifelse(is.na(result$Count_Use_Count), 0, result$Count_Use_Count))


#fix some weird values in lat and lon coordinates
f <- result[result$DECSLON > 0 | result$DECSLAT > 35,]
f <- f[!is.na(f$STATIONID),]
fid <- f$STATIONID
result <- result[result$STATIONID %!in% fid, ]

f <- ddply(f, .(STATIONID), function(h){

  #DECSLON <- as.numeric(getElement(h, "DECSLON"))

  if(!is.na(h$DECSLON)){

    if(h$DECSLON > 35){

      h$DECELAT <- NA
      h$DECSLON <- NA
      h$DECELAT <- NA
      h$DECELON <- NA

    } else {

      h$DECSLAT <- h$DECSLON
      h$DECSLON <- h$DECELAT
      h$DECELAT <- h$DECELON
      h$DECELON <- NA
    }
  }

  if(!is.na(h$DECELON)){

    if(h$DECELON > 0){
      h$DECELON <- NA
    }

  }

  if(!is.na(h$DECSLON)){

    if(h$DECSLON > 0){
      h$DECSLON <- NA
    }
  }

  if(!is.na(h$DECELAT)){

    if(h$DECELAT < 0){
      h$DECELAT <- NA
    }
  }

  if(!is.na(h$DECSLAT)){

    if(h$DECSLAT > 35){
      h$DECSLAT <- NA
    }
  }

  return(h)
}, .progress = "text", .inform = T)

f2 <- result[result$DECELON > 0 | result$DECELAT < 10,]
f2 <- f2[!is.na(f2$STATIONID),]
fid2 <- f2$STATIONID

'%!in%' <- function(x,y)!('%in%'(x,y))
#remove stations with incorrect DECELAT
result <- result[result$STATIONID %!in% fid2, ]
result <- rbind(result, f)

result$taxa <- taxa[1]

#re-order columns
result <- result %>% select(taxa, AggGrp_20130430, Habitat, Season, Year, Month, Day, Use_Depth_m, DECSLAT, DECSLON, DECELAT, DECELON, everything())


write.csv(x = result, file = paste0(taxa[1], "_SEAMAP.csv"), row.names = F)
