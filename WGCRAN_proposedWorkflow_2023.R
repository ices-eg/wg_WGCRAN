# WG CRAN

# proposed workflow for WGCRAN data call on fishing effort of TBB_CRU_16-31

# WG-CRAN

# (proposed workflow to calculate fishing effort

# A) from efalo-format: calculate hours at sea (departure and return datetime, HAS), kwHAS, "fishing hours of hauls" (from log information, fiHAShaul), kwfiHAShaul,    landings of CSH (kg), N vessels
# B) from ICES VMS data call: fishing effort (hours) from VMS activity (activity windows from speed, fiHASvms), kwfiHASvms, landings of all species (kg, tons)


# author: Torsten Schulze, Thuenen-Institute of Sea Fisheries
# 20220626 modified for NL DATA by U. Beier 
# 20220628 modified to read table1 in ICES requested format, i.e. specific order of columns, no column name!!!!

library(data.table)
library(dplyr)

cat("\014")                     # clears console
rm(list=ls()); gc()             # clean up the workspace


#years<- c(2009:2021)

oma<-Sys.time()
tmstmp<-paste(substr(oma,1,4),substr(oma,6,7),substr(oma,9,10),"_",substr(oma,12,13),substr(oma,15,16),"s",substr(oma,18,19),sep="");rm(oma)
tmstmp

# set pathes
tacefpath    <- "/your.Path"    # EFLALO (and tacsat) path
icestab1path <- "/your.Path"    # Ices call table 1 path
resultPath   <- "/your.Path"

country<-"DEU"                  # set your country


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#A) from eflalo ####


#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#B) from ICES VMS data call, table1 ####
# functions (from WGSFD QC check)
d2ir <- function (lon, lat = NULL, useI = FALSE) {
  
  # if lon is a dataframe
  if (is.null(lat)) {
    lon <- lat$lon
    lat <- lat$lat
  }
  lat <- lat + 1e-06
  lon <- lon + 1e-06
  outside <- lat < 36 | lat >= 85.5 | lon <= -44 | lon > 68.5
  if (any(outside)) 
    warning("Positions outside of ICES statistical area")
  lat <- floor(lat * 2) - 71
  lat <- ifelse(lat < 10, paste("0", lat, sep = ""), lat)
  if (useI) 
    lettersUsed <- LETTERS[1:12]
  else lettersUsed <- LETTERS[c(1:8, 10:13)]
  lon1 <- lettersUsed[(lon + 60)%/%10]
  lon2 <- ifelse(lon1 == "A", floor(lon%%4), floor(lon%%10))
  ir <- paste(lat, lon1, lon2, sep = "")
  ir[outside] <- NA
  ir
}
ir2d <- function (ir, useI = FALSE) {
  lat <- substring(ir, 1, 2)
  lat <- as.numeric(lat)
  lat <- (lat + 71)/2 + 0.25
  lon1 <- substring(ir, 3, 3)
  lon1 <- toupper(lon1)
  lon1 <- match(lon1, LETTERS)
  if (!useI) 
    lon1 <- ifelse(lon1 > 8, lon1 - 1, lon1)
  lon1 <- lon1 - 2
  lon2 <- substring(ir, 4)
  lon2 <- as.numeric(lon2)
  lon <- ifelse(lon1 < 0, -44 + lon2 + 0.5, -40 + 10 * lon1 + 
                  lon2 + 0.5)
  data.frame(lat = lat, lon = lon)
}
csq2pos <- function(d, dx = 0.05) {
  bind_cols(d,
            vmstools::CSquare2LonLat(d$csq, degrees = dx) %>% 
              select(lon = SI_LONG,
                     lat = SI_LATI)) %>% 
    as_tibble()
}
read_ve <- function(file) {
  read.csv(file,
           header = FALSE,
           na.strings = "NULL") %>% 
    tidyr::as_tibble() %>% 
    dplyr::rename(type = 1,
                  country = 2,
                  year = 3,
                  month = 4,
                  n_vessel = 5,
                  vids = 6,
                  csq = 7,
                  dcf4 = 8,
                  dcf5 = 9,
                  dcf6 = 10,
                  lclass = 11,
                  speed = 12,
                  effort = 13,             # units in hours
                  length = 14,             # average vessel length
                  kw = 15,                 # average kw
                  kwh = 16,                # kw x effort
                  catch = 17,
                  value = 18,
                  spread = 19) %>% 
    dplyr::mutate(value = as.numeric(value),
                  csq = as.character(csq)
    )
}
ve <- 
  #read_ve("/media/fssf01bhv/vmsdata/DataGlobal/deu2ices/2020/table1_DE_v20210505_220608_2009-2020.csv") %>% 
  read_ve("/media/sfsfs001vbhv/vmsdata/Projects/ICES_vmsDataCall_2022/runCvD_220525/table1Save.csv") %>% 
  csq2pos() %>% 
  # NEED TO CHECK THIS UPSTREAM
  filter(!is.na(lon)) %>% 
  mutate(isq = d2ir(lat, lon))


#load(file="W:/IMARES/Data/VMS_/2022/Datacall_ICES/OutPut/table1Save.RData")
#load(file=paste(icestab1path, "table1Save.RData", sep=""))

#table10 <- data.frame(table1Save)
table10 <- data.frame(ve)
names(table10)
#TBB_CRU_16-31_0_0
table1  <- subset(table10, table10$dcf6 == "TBB_CRU_16-31_0_0") # Selects metier
aggr<- do.call(data.frame, aggregate(cbind(table1$catch, table1$effort, table1$kwh), 
                                     list(Month=table1$month, Year=table1$year
                                     ),
                                     FUN=sum))
aggr$TotWeightkg <-  aggr$V1
aggr$fiHASvms    <-  aggr$V2
aggr$kwfiHASvms  <-  aggr$V3
aggr$Country     <- country
aggr$V1          <- NULL
aggr$V2          <- NULL
aggr$V3          <- NULL
names(aggr)
aggr_write <- aggr[c("Country", "Year", "Month", "TotWeightkg", "fiHASvms", "kwfiHASvms")]

aggrpy_write <- aggr_write %>%
  group_by(Year) %>%
  summarize(TotWeightkg=sum(TotWeightkg), fiHASvms=sum(fiHASvms), kwfiHASvms=sum(kwfiHASvms))

write.csv(aggr_write,   file=file.path(paste(resultPath, "/monthly_landings_VMS_fishing_hours_", country,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")
write.csv(aggrpy_write, file=file.path(paste(resultPath, "/yearly_landings_VMS_fishing_hours_",  country,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")


