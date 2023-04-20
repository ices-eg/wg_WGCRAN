# WG CRAN

# proposed workflow for WGCRAN data call on fishing effort of TBB_CRU_16-31

# WG-CRAN

# (proposed workflow to calculate fishing effort

# A) from efalo-format: calculate hours at sea (departure and return datetime, HAS), kwHAS, "fishing hours of hauls" (from log information, fiHAShaul), kwfiHAShaul,    landings of CSH (kg), N vessels
# B) from ICES VMS data call: fishing effort (hours) from VMS activity (activity windows from speed, fiHASvms), kwfiHASvms, landings of all species (kg, tons)


# author: Torsten Schulze, Thuenen-Institute of Sea Fisheries
# 20220626 modified for NL DATA by U. Beier 
# 20220628 modified to read table1 in ICES requested format, i.e. specific order of columns, no column name!!!!

cat("\014")                     # clears console
# rm(list=ls()); gc()             # clean up the workspace

library(data.table)
library(dplyr)


years<- c(2009:2021)

oma<-Sys.time()
tmstmp<-paste(substr(oma,1,4),substr(oma,6,7),substr(oma,9,10),"_",substr(oma,12,13),substr(oma,15,16),sep="");rm(oma)
tmstmp

country<-"DEU"                  # set your country

# set pathes
# tacefpath    <- "/your.Path"    # EFLALO (and tacsat) path
# icestab1path <- "/your.Path"    # Ices call table 1 path
# resultPath   <- "/your.Path"    # write the national tables here

# read pathes from non public file ####
getwd()
if(country == "DEU") source("./pathes.R")     



#ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# A) from ICES VMS data call, table 1 ####
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
                                     list(month=table1$month, year=table1$year
                                     ),
                                     FUN=sum))


aggr$TotWeightkg     <-  aggr$V1
aggr$fihrsAtSeaVMS   <-  aggr$V2
aggr$kwfihrsAtSeaVMS <-  aggr$V3
aggr$hp_fihrsAtSeaVMS <-  aggr$kwfihrsAtSeaVMS*1.357

aggr$country <- country
aggr$V1 <- NULL
aggr$V2 <- NULL
aggr$V3 <- NULL
aggr$kwfihrsAtSeaVMS <- NULL
names(aggr)
aggr_write <- aggr[c("country", "year", "month", "TotWeightkg", "fihrsAtSeaVMS", "hp_fihrsAtSeaVMS")]

aggrpy_write <- aggr_write %>%
  group_by(year) %>%
  summarize(TotWeightkg=sum(TotWeightkg), fihrsAtSeaVMS=sum(fihrsAtSeaVMS), hp_fihrsAtSeaVMS=sum(hp_fihrsAtSeaVMS))


# ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
# B) the EFLALO derived data ####


results_yr<-NULL
results_mnth<-NULL

# test year: myear <- 2010
for(myear in years){
  print(myear)
  
  load(file = paste0(tacefpath, "/cleanEflalo_",myear,".RData"))
  #load(file.path(tacefpath, paste('cleanEflalo_', myear, '.RData', sep = '')))
  
  
  eflalo <- data.frame(eflalo)
  # load(file.path(tacefpath, paste('cleanEflalo_', myear, '.RData',sep='')))
  eflalo_00<-eflalo # start anew: eflalo<-eflalo_00
  
  eflalo$year<-year(eflalo$FT_DDATIM) # Starting the trip
  eflalo$month<-month(eflalo$FT_DDATIM) # Starting the trip
  
  #check: unique(eflalo$year)
  
  # get time at sea ####
  eflalo$minutes_at_sea <- difftime(eflalo$FT_LDATIM, eflalo$FT_DDATIM,units="mins") # use of same time formats!  e.g. UTC (time in electronic log books)
  eflalo$has            <- round(as.numeric(eflalo$minutes_at_sea)/60,1); #hours_at_sea
  eflalo$das            <- round(as.numeric(eflalo$minutes_at_sea)/1400,1); #days_at_sea
  
  #get fishing time per haul ####
  eflalo$minutes_per_haul <- difftime(eflalo$LE_EDATIM, eflalo$LE_SDATIM,units="mins") # use of same time formats!  e.g. UTC (time in electronic log books)
  eflalo$fihas            <- round(as.numeric(eflalo$minutes_per_haul)/60,1); #hours_at_sea
  
  # extract: hauls catching CSH, using shrimp gear (TBB_16-31), trip length below 5days (<120hrs), vessel length above 10m
  # TBC: synonym for beam trawls (TBB) catching CSH (crangon), i.e. TBC is TBB_CRU_16-31, only in German data
  eflalo$LE_GEAR[eflalo$LE_GEAR=="TBC"] <-"TBB"
  eflalo_csh_isTBC_b5d_a10m<-subset(eflalo,  LE_KG_CSH>0 & LE_GEAR =="TBB" & LE_MSZ>16 & LE_MSZ<=31 & has<120 & VE_LEN>=10 ) 
  
  # in German data there are some funny mesh sizes <16mm catching CSH with TBB
  eflalo_csh_isTBCX_b5d_a10m<-subset(eflalo,  LE_KG_CSH>0 & LE_GEAR =="TBB" & LE_MSZ<16  & has<120 & VE_LEN>=10 ) 
  cat("\n")
  print(paste("N rows eflalo:",nrow(eflalo)))
  print(paste("N rows eflalo_csh_isTBC1631_b5d_a10m:", nrow(eflalo_csh_isTBC_b5d_a10m)))
  print(paste("N rows eflalo_csh_isTBC<16_b5d_a10m:", nrow(eflalo_csh_isTBCX_b5d_a10m)))
  print(paste("vessels in eflalo_csh_isTBC<16_b5d_a10m:", unique(eflalo_csh_isTBCX_b5d_a10m$VE_REF)))
  
  cat("\n");  cat("\n")
  
  
  ## make sum per year and sum per month ####
  # landings
  aggr_kg_yr<-aggregate(eflalo_csh_isTBC_b5d_a10m$LE_KG_CSH, list(year=eflalo_csh_isTBC_b5d_a10m$year), FUN=sum)
  names(aggr_kg_yr)[names(aggr_kg_yr)=="x"]<-"kg_csh"
  aggr_kg_mnth<-aggregate(eflalo_csh_isTBC_b5d_a10m$LE_KG_CSH, list(year=eflalo_csh_isTBC_b5d_a10m$year, month=eflalo_csh_isTBC_b5d_a10m$month), FUN=sum)
  names(aggr_kg_mnth)[names(aggr_kg_mnth)=="x"]<-"kg_csh"
  
  # hours at sea
  # for time at sea remove duplicates (serveral log events per trip) first
  eflalo_csh_isTBC_b5d_a10m$AgLev <- paste(eflalo_csh_isTBC_b5d_a10m$VE_REF, eflalo_csh_isTBC_b5d_a10m$FT_REF, eflalo_csh_isTBC_b5d_a10m$FT_DDAT, sep="")
  eflalo_csh_isTBC_b5d_a10m_nodup <- distinct(eflalo_csh_isTBC_b5d_a10m, AgLev, .keep_all = TRUE); 
  aggr_has_yr<-aggregate(eflalo_csh_isTBC_b5d_a10m_nodup$has, list(year=eflalo_csh_isTBC_b5d_a10m_nodup$year), FUN=sum)
  names(aggr_has_yr)[names(aggr_has_yr)=="x"]<-"hrsAtSea"
  aggr_has_mnth<-aggregate(eflalo_csh_isTBC_b5d_a10m_nodup$has, list(year=eflalo_csh_isTBC_b5d_a10m_nodup$year,month=eflalo_csh_isTBC_b5d_a10m_nodup$month), FUN=sum)
  names(aggr_has_mnth)[names(aggr_has_mnth)=="x"]<-"hrsAtSea"
  
  # fishing hours at sea (fishing effort from haul information - start/end: LE_SDATIM/LE_EDATIM)
  aggr_fihas_yr <- aggregate(eflalo_csh_isTBC_b5d_a10m$fihas, list(year=eflalo_csh_isTBC_b5d_a10m$year), FUN=sum)
  names(aggr_fihas_yr)[names(aggr_fihas_yr)=="x"]<-"fihrsAtSea"
  aggr_fihas_mnth<-aggregate(eflalo_csh_isTBC_b5d_a10m$fihas, list(year=eflalo_csh_isTBC_b5d_a10m$year, month=eflalo_csh_isTBC_b5d_a10m$month), FUN=sum)
  names(aggr_fihas_mnth)[names(aggr_fihas_mnth)=="x"]<-"fihrsAtSea"
  
  # horse power day at sea
  eflalo_csh_isTBC_b5d_a10m_nodup$hp_has <- eflalo_csh_isTBC_b5d_a10m_nodup$VE_KW*1.357466*eflalo_csh_isTBC_b5d_a10m_nodup$has
  aggr_hphas_yr<-aggregate(eflalo_csh_isTBC_b5d_a10m_nodup$hp_has, list(year=eflalo_csh_isTBC_b5d_a10m_nodup$year), FUN=sum)
  names(aggr_hphas_yr)[names(aggr_hphas_yr)=="x"]<-"hp_hrsAtSea"
  aggr_hphas_mnth<-aggregate(eflalo_csh_isTBC_b5d_a10m_nodup$hp_has, list(year=eflalo_csh_isTBC_b5d_a10m_nodup$year, month=eflalo_csh_isTBC_b5d_a10m_nodup$month), FUN=sum)
  names(aggr_hphas_mnth)[names(aggr_hphas_mnth)=="x"]<-"hp_hrsAtSea"
  
  # horse power fishing effort per haul
  eflalo_csh_isTBC_b5d_a10m$hp_fihas <- eflalo_csh_isTBC_b5d_a10m$VE_KW*1.357466*eflalo_csh_isTBC_b5d_a10m$fihas
  aggr_hpfihas_yr<-aggregate(eflalo_csh_isTBC_b5d_a10m$hp_fihas, list(year=eflalo_csh_isTBC_b5d_a10m$year), FUN=sum)
  names(aggr_hpfihas_yr)[names(aggr_hpfihas_yr)=="x"]<-"hp_fihrsAtSea"
  aggr_hpfihas_mnth<-aggregate(eflalo_csh_isTBC_b5d_a10m$hp_fihas, list(year=eflalo_csh_isTBC_b5d_a10m$year, month=eflalo_csh_isTBC_b5d_a10m$month), FUN=sum)
  names(aggr_hpfihas_mnth)[names(aggr_hpfihas_mnth)=="x"]<-"hp_fihrsAtSea"
  
  # number of vessels
  n_ves_mnth <- aggregate(VE_REF ~ year + month, data=eflalo_csh_isTBC_b5d_a10m_nodup, FUN=function(x) length(unique(x)));
  n_ves_yr   <- aggregate(VE_REF ~ year, data=eflalo_csh_isTBC_b5d_a10m_nodup, FUN=function(x) length(unique(x)));
  names(n_ves_yr)[names(n_ves_yr)=="VE_REF"]<-"n_vessels"
  names(n_ves_mnth)[names(n_ves_mnth)=="VE_REF"]<-"n_vessels"
  
  # number of trips
  n_trps_mnth <- aggregate(FT_REF ~ year + month, data=eflalo_csh_isTBC_b5d_a10m_nodup, FUN=function(x) length(unique(x)));
  n_trps_yr   <- aggregate(FT_REF ~ year, data=eflalo_csh_isTBC_b5d_a10m_nodup, FUN=function(x) length(unique(x)));
  names(n_trps_yr)[names(n_trps_yr)=="FT_REF"]<-"n_trips"
  names(n_trps_mnth)[names(n_trps_mnth)=="FT_REF"]<-"n_trips"
  
  rm(aggr_all_yr)
  aggr_all_yr<-merge(aggr_has_yr, aggr_hphas_yr )
  aggr_all_yr<-merge(aggr_all_yr, aggr_fihas_yr )
  aggr_all_yr<-merge(aggr_all_yr, aggr_hpfihas_yr )
  aggr_all_yr<-merge(aggr_all_yr, aggr_kg_yr)
  aggr_all_yr<-merge(aggr_all_yr, n_ves_yr)
  aggr_all_yr<-merge(aggr_all_yr, n_trps_yr)
  
  rm(aggr_all_mnth)
  aggr_all_mnth<-merge(aggr_has_mnth, aggr_hphas_mnth,    by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-merge(aggr_all_mnth, aggr_fihas_mnth,    by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-merge(aggr_all_mnth, aggr_hpfihas_mnth,  by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-merge(aggr_all_mnth, aggr_kg_mnth,       by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-merge(aggr_all_mnth, n_ves_mnth,         by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-merge(aggr_all_mnth, n_trps_mnth,        by.x=c("year", "month"), by.y=c("year", "month") )
  aggr_all_mnth<-aggr_all_mnth[order(aggr_all_mnth$month),]
  
  results_yr<-rbind(results_yr, aggr_all_yr)
  results_mnth<-rbind(results_mnth, aggr_all_mnth)
}#end of loop over years



# combine eflalo and table 1 derived data

results_yr2<- cbind(results_yr, aggrpy_write)
results_yr2$country<-country
results_yr2<-results_yr2[, c("country", "year", "hrsAtSea", "hp_hrsAtSea", "fihrsAtSea", "hp_fihrsAtSea", "kg_csh", "n_vessels", "n_trips", "TotWeightkg", "fihrsAtSeaVMS", "hp_fihrsAtSeaVMS")]

results_mnth2<- cbind(results_mnth, aggr_write)
results_mnth2<-results_mnth2[, c("country", "year", "month", "hrsAtSea", "hp_hrsAtSea", "fihrsAtSea", "hp_fihrsAtSea", "kg_csh", "n_vessels", "n_trips", "TotWeightkg", "fihrsAtSeaVMS", "hp_fihrsAtSeaVMS")]


#oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# check and write results ####



#write csv files, both tables joint together ####
write.csv(results_yr2,    file=file.path(paste(resultPath, "/results_WGCRAN_prpWrkfl_eflaloVMS_Yr_",    country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")
write.csv(results_mnth2,  file=file.path(paste(resultPath, "/results_WGCRAN_prpWrkfl_eflaloVMS_Mnth_",  country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")

# write excel files on linux server 
# library(openxlsx);  
# write.xlsx(results_yr,    file=paste0(resultPath,"/results_WGCRAN_prpWrkfl_eflalo_Yr_",  country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".xlsx"), sheetName = "WGCRAN", col.names = TRUE, row.names = FALSE, append = FALSE)
# write.xlsx(results_mnth,  file=paste0(resultPath,"/results_WGCRAN_prpWrkfl_eflalo_Mnth_",country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".xlsx"), sheetName = "WGCRAN", col.names = TRUE, row.names = FALSE, append = FALSE)




# 
# 
# # table 1
# write.csv(aggr_write,   file=file.path(paste(resultPath, "/monthly_landings_VMS_fishing_hours_", country,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")
# write.csv(aggrpy_write, file=file.path(paste(resultPath, "/yearly_landings_VMS_fishing_hours_",  country,".csv",sep="")), quote = TRUE, row.names = FALSE, na="")
# 
# 
# # EFLALO
# View(results_yr)
# View(results_mnth)
# write.csv(results_yr,    file=paste0(resultPath,"results_WGCRAN_prpWrkfl_eflalo_Yr_"  ,country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".csv"))
# write.csv(results_mnth,  file=paste0(resultPath,"results_WGCRAN_prpWrkfl_eflalo_Mnth_",country,"_",tmstmp,"_",  years[1],"-",years[length(years)] ,".csv"))



