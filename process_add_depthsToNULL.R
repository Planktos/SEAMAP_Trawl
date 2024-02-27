
f <- list.files(recursive = T, pattern = ".txt")

d <- read.csv("jelly_inshore_20m_NULL_depth_4km_join.txt", header = T, stringsAsFactors = F)

# INVREC = read.csv(file = "20190329_SEAMAP_csv/INVREC.csv")
# ISTREC = read.csv(file = "20190329_SEAMAP_csv/ISTREC.csv")
#NEWBIOCODESBIG = read.csv(file = "20190329_SEAMAP_csv/NEWBIOCODESBIG.csv")
#SHRREC = read.csv(file = "20190329_SEAMAP_csv/SHRREC.csv")
VESSELS = read.csv('VESSELS.csv')
CTDCASTREC = read.csv('CTDCASTREC.csv')
CTDREC = read.csv('CTDREC.csv')
GLFREC = read.csv('GLFREC.csv')
BGSREC = read.csv('BGSREC.csv')
ENVREC = read.csv('ENVREC.csv')
CRUISES = read.csv('CRUISES.csv')

#functions -----
clean.quotes <- function(y){
  gsub(pattern = '"', replacement = "", x = y)
}

clean.whitespace<- function(y){
  gsub(pattern = ' ', replacement = "", x = y)
}

s = read.csv(file = "20190329_SEAMAP_csv/STAREC_rev20190402.csv", sep = ",", stringsAsFactors = F, quote = "", fill = T)

#get field names
s.names = read.csv(file = "20190329_SEAMAP_csv/STAREC.csv", stringsAsFactors = F, header = T)
names(s) <- names(s.names)
rm(s.names)

#clean quotes off of data
s[] <- sapply(X = s, FUN = clean.quotes)
s$GEARS <- sapply(X = s$GEARS, FUN = clean.whitespace)


j <- fread("jelly_inshore_20m_NULL_depth_4km_join.txt", stringsAsFactors = F, header = T)
ji <- j[,c("stationid", "depth_m_12","year", "month", "day")]
ji <- rename(ji, c("depth_m_12" = "Use_Depth_m_Arc"))
ji <- rename(ji, c("stationid" = "STATIONID"))
ji <- rename(ji, c("year" = "Year"))
ji <- rename(ji, c("month" = "Month"))
ji <- rename(ji, c("day" = "Day"))

j <- fread("jelly_shelf_200m_NULL_depth_4km_join.txt", stringsAsFactors = F, header = T)
js <- j[,c("stationid", "depth_m_12","year", "month", "day")]
js <- rename(js, c("depth_m_12" = "Use_Depth_m_Arc"))
js <- rename(js, c("stationid" = "STATIONID"))
js <- rename(js, c("year" = "Year"))
js <- rename(js, c("month" = "Month"))
js <- rename(js, c("day" = "Day"))

ja <- rbind(ji,js)
