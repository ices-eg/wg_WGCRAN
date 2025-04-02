# Skript for  WGCRAN
# Written by: Miriam P?ts & Leander H?hne & Kim H?nerlage (Th?nen Institute Bremerhaven)
# First version from  17.10.22 10:00 PM ... ;-) and optimized over time
# This skript should create an output of 19 x .csv-files & XY Figures (still under construction)

remove(list = ls())
gc()

library(tidyverse)
library(tidyr)

#Automatical time stamp and file naming 
current.Year<-format(Sys.time(), "%d%m%Y")
meetingYear <- "2024"                                  # put year of WGCRAN meeting
dataYear <- "2023"                                     # enter year of data origin = generally year before

##Set working directory and read in the data
setwd("D://") #specify the folder in which you have saved the file that you want to import




#### Create Summary Table ####
# FOR UPDATE CHECK NAME OF THE FILE
# INFO: data in csv-file were compiled by another R script by combining the countries' data deliveries

dat<-read.csv("UPDATE WGCRAN effort and landings 1950-2023 update 16092024.csv",header=TRUE,sep=",") #when csv2 check for sep=";"!
str(dat)
dat$Country<-as.factor(dat$Country) #in console "str(dat)" gives country as chr = character BUT should be as factor
#dat$Landings<-as.numeric(dat$Landings)            #should be in num
#dat$Effort_HPDAYS<-as.numeric(dat$Effort_HPDAYS)  #should be in num
#dat$Effort_DAS<-as.numeric(dat$Effort_DAS)        #should be in num
#dat$FiDAS<-as.numeric(dat$FiDAS)                  #should be in num
#dat$hp_FiDAS<-as.numeric(dat$hp_FiDAS)            #should be in num

# Calculate LPUE indices, Steam time DAS and steam time HPDAS and add them to the table
dat$LPUE_DAS <- (dat$Landings / dat$Effort_DAS) * 1000
dat$LPUE_HPDAS <- (dat$Landings / dat$Effort_HPDAYS) * 1000
dat$steam_DAS <- (dat$Effort_DAS - dat$FiDAS)
dat$steam_HPDAS <- (dat$Effort_HPDAYS - dat$hp_FiDAS)

write.csv(dat, paste0("WGCRAN",meetingYear,"_All_Summary_Landings_Effort_LPUE_FiTime_", min(dat$Year), "-", max(dat$Year), "_","update", current.Year, ".csv"), row.names = FALSE)


#### A N N U A L  D A T A: Tables behind Figures 1,2,3,4 & 5 ####

##### LANDINGS per country (inkl. % share) ####
tmp<-aggregate(Landings~Country+Year,data=dat,FUN=sum) #calculate annual landings
data_wide_tmp <- spread(tmp, Country, Landings) # to get a wide table
data_wide_tmp <- data_wide_tmp %>%              # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "France" = "FR",
         "Germany" = "GE",
         "Netherlands" = "NL",
         "United_Kingdom" = "UK")
data_wide_sort <- data_wide_tmp [,c("Year","Germany", "Netherlands", "Denmark", "Belgium", "United_Kingdom", "France")]

#Percentage (maybe could be simplified but I am too tired ;-D)
data_wide_sort$Germany_per<-(data_wide_sort$Germany/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100
data_wide_sort$Netherlands_per<-(data_wide_sort$Netherlands/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100
data_wide_sort$Denmark_per<-(data_wide_sort$Denmark/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100
data_wide_sort$Belgium_per<-(data_wide_sort$Belgium/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100 
data_wide_sort$United_Kingdom_per<-(data_wide_sort$United_Kingdom/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100
data_wide_sort$France_per<-(data_wide_sort$France/rowSums(data_wide_sort[,2:7],na.rm=TRUE))*100

write.csv(data_wide_sort,paste0("WGCRAN",meetingYear,"_Fig.1 Annual Landings ", min(tmp$Year),"-", max(tmp$Year), "_","update", current.Year, ".csv"), row.names=FALSE)
write.csv(data_wide_sort,paste0("WGCRAN",meetingYear,"_Fig.5 Annual Landings with % ", min(tmp$Year),"-", max(tmp$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### PLOT: stacked graph Time series of total landings DRAFT! ####
library(ggplot2)
tmp_73<-subset(tmp,Year>=1990)
my_title <- expression(paste(italic("Crangon crangon"), " commercial landings since 1990"))

ggplot(data=tmp_73, aes(x=Year, y=Landings, fill=Country)) +
  geom_bar(stat="identity")+ theme_classic()+ labs(title=my_title, 
                                                   x="Year", y = "Landings per country in tonnes")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  #scale_fill_brewer(palette="PuBuGn")
  scale_fill_brewer(palette="Reds")

##### DAS: days at sea all countries (inkl. % share) ####
tmp_DAS<-aggregate(Effort_DAS~Country+Year,data=dat,FUN=sum)     # calculate annual effort DAS
data_wide_tmp_DAS <- spread(tmp_DAS, Country, Effort_DAS)        # again to wide table
data_wide_tmp_DAS <- data_wide_tmp_DAS %>%                       # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "France" = "FR",
         "Germany" = "GE",
         "Netherlands" = "NL",
         "United_Kingdom" = "UK")
data_wide_sort_DAS <- data_wide_tmp_DAS [,c("Year","Germany", "Netherlands", "Denmark", "Belgium", "United_Kingdom", "France")]

#Percentage (maybe could be simplified but I am too tired ;-D)
data_wide_sort_DAS$German_per<-(data_wide_sort_DAS$German/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100
data_wide_sort_DAS$Netherlands_per<-(data_wide_sort_DAS$Netherlands/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100
data_wide_sort_DAS$Denmark_per<-(data_wide_sort_DAS$Denmark/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100
data_wide_sort_DAS$Belgium_per<-(data_wide_sort_DAS$Belgium/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100 
data_wide_sort_DAS$United_Kingdom_per<-(data_wide_sort_DAS$United_Kingdom/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100
data_wide_sort_DAS$France_per<-(data_wide_sort_DAS$France/rowSums(data_wide_sort_DAS[,2:7],na.rm=TRUE))*100

write.csv(data_wide_sort_DAS,paste0("WGCRAN",meetingYear,"_Fig.2a Annual DAS ",min(tmp_DAS$Year),"-", max(tmp_DAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### HPDAS: horse power days at sea per country (inkl. % share) ####
tmp_HPDAS<-aggregate(Effort_HPDAYS~Country+Year,data=dat,FUN=sum)  # calculate annual effort HPDAYS
data_wide_tmp_HPDAS <- spread(tmp_HPDAS, Country, Effort_HPDAYS)   # to get a wide table
data_wide_tmp_HPDAS <- data_wide_tmp_HPDAS %>%                     # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "France" = "FR",
         "Germany" = "GE",
         "Netherlands" = "NL",
         "United_Kingdom" = "UK")
data_wide_sort_HPDAS <- data_wide_tmp_HPDAS [,c("Year","Germany", "Netherlands", "Denmark", "Belgium", "United_Kingdom", "France")]

#Percentage (maybe could be simplified but I am too tired ;-D)
data_wide_sort_HPDAS$German_per<-(data_wide_sort_HPDAS$Germany/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100
data_wide_sort_HPDAS$Netherlands_per<-(data_wide_sort_HPDAS$Netherlands/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100
data_wide_sort_HPDAS$Denmark_per<-(data_wide_sort_HPDAS$Denmark/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100
data_wide_sort_HPDAS$Belgium_per<-(data_wide_sort_HPDAS$Belgium/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100 
data_wide_sort_HPDAS$United_Kingdom_per<-(data_wide_sort_HPDAS$United_Kingdom/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100
data_wide_sort_HPDAS$France_per<-(data_wide_sort_HPDAS$France/rowSums(data_wide_sort_HPDAS[,2:7],na.rm=TRUE))*100

write.csv(data_wide_sort_HPDAS,paste0("WGCRAN",meetingYear,"_Fig.2b Annual hp_DAS ",min(tmp_HPDAS$Year),"-", max(tmp_HPDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### FiDAS: Fishing time in DAS per country  ####  in 2024 it was decided to have the data reasy but only report in hp_DAS
tmp_FiDAS<-aggregate(FiDAS~Country+Year,data=dat,FUN=sum)     # calculate annual effort from VMS FiDAS
data_wide_tmp_FiDAS <- spread(tmp_FiDAS, Country, FiDAS)      # to get wide table
data_wide_tmp_FiDAS <- data_wide_tmp_FiDAS %>%                # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "Germany" = "GE",
         "Netherlands" = "NL")
data_wide_sort_FiDAS <- data_wide_tmp_FiDAS [,c("Year","Germany", "Netherlands", "Denmark", "Belgium")]

write.csv(data_wide_sort_FiDAS,paste0("WGCRAN",meetingYear,"_Annual DAS Fishing ", min(tmp_FiDAS$Year),"-", max(tmp_FiDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### Steamtime in DAS per country ####  in 2024 it was decided to have the data reasy but only report in hp_DAS
tmp_steam_DAS<-aggregate(steam_DAS~Country+Year,data=dat,FUN=sum)      # calculate annual steam time steam_DAS
data_wide_tmp_steam_DAS <- spread(tmp_steam_DAS, Country, steam_DAS)   # to get wide table
data_wide_tmp_steam_DAS <- data_wide_tmp_steam_DAS %>%                 # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "Germany" = "GE",
         "Netherlands" = "NL")
data_wide_sort_steam_DAS <- data_wide_tmp_steam_DAS [,c("Year","Germany", "Netherlands", "Denmark","Belgium")]

write.csv(data_wide_sort_steam_DAS,paste0("WGCRAN",meetingYear,"_Annual DAS Steaming ", min(tmp_steam_DAS$Year),"-", max(tmp_steam_DAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### hp_FiDAS: Fishing time in hp_DAS per country ####
tmp_hp_FiDAS<-aggregate(hp_FiDAS~Country+Year,data=dat,FUN=sum)    # calculate annual effort from VMS hp_FiDAS
data_wide_tmp_hp_FiDAS <- spread(tmp_hp_FiDAS, Country, hp_FiDAS)  # to get wide table
data_wide_tmp_hp_FiDAS <- data_wide_tmp_hp_FiDAS %>%               # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "Germany" = "GE",
         "Netherlands" = "NL")
data_wide_sort_hp_FiDAS <- data_wide_tmp_hp_FiDAS [,c("Year","Germany", "Netherlands", "Denmark","Belgium")]

write.csv(data_wide_sort_hp_FiDAS,paste0("WGCRAN",meetingYear,"_Fig.3 Annual hp_DAS Fishing ", min(tmp_hp_FiDAS$Year),"-", max(tmp_hp_FiDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### Steamtime in hp_DAS per country ####
tmp_steam_HPDAS<-aggregate(steam_HPDAS~Country+Year,data=dat,FUN=sum)      # calculate annual steamtime steam_HPDAS
data_wide_tmp_steam_HPDAS <- spread(tmp_steam_HPDAS, Country, steam_HPDAS) # to get wide table
data_wide_tmp_steam_HPDAS <- data_wide_tmp_steam_HPDAS %>%                 # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "Germany" = "GE",
         "Netherlands" = "NL")
data_wide_sort_steam_HPDAS <- data_wide_tmp_steam_HPDAS [,c("Year","Germany", "Netherlands", "Denmark","Belgium")]

write.csv(data_wide_sort_steam_HPDAS,paste0("WGCRAN",meetingYear,"_Fig.3 Annual hp_DAS Steaming ", min(tmp_steam_HPDAS$Year),"-", max(tmp_steam_HPDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### LPUE kg per DAS per country #### in 2024 it was decided to have the data reasy but only report in hp_DAS
dat_LPUEDAS<-merge(tmp,tmp_DAS,by=c("Year","Country"),all.x=TRUE)
dat_LPUEDAS$LPUEDAS <- (dat_LPUEDAS$Landings/dat_LPUEDAS$Effort_DAS)*1000
dat_LPUEDAS_S<-dat_LPUEDAS[,c("Country", "Year", "LPUEDAS")]
dat_LPUEDAS_S2<-dat_LPUEDAS_S[!is.na(dat_LPUEDAS_S$LPUEDAS),]

data_wide_LPUEDAS <- spread(dat_LPUEDAS_S2, Country, LPUEDAS)    # to get wide table
data_wide_LPUEDAS <- data_wide_LPUEDAS %>%                       # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "France" = "FR",
         "Germany" = "GE",
         "Netherlands" = "NL",
         "United_Kingdom" = "UK")
data_wide_sort_LPUEDAS <- data_wide_LPUEDAS [,c("Year","Germany", "Netherlands", "Denmark", "Belgium", "United_Kingdom", "France")]

write.csv(data_wide_sort_LPUEDAS,paste0("WGCRAN",meetingYear,"_Annual LPUE kg per DAS ", min(data_wide_sort_LPUEDAS$Year),"-", max(data_wide_sort_LPUEDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)

##### LPUE kg per hp_DAS per country ####
dat_LPUEHPDAS<-merge(tmp,tmp_HPDAS,by=c("Year","Country"),all.x=TRUE)
dat_LPUEHPDAS$LPUE_HPDAS <- (dat_LPUEHPDAS$Landings/dat_LPUEHPDAS$Effort_HPDAYS)*1000
dat_LPUEHPDAS_S<-dat_LPUEHPDAS[,c("Country", "Year", "LPUE_HPDAS")]
dat_LPUEHPDAS_S2<-dat_LPUEHPDAS_S[!is.na(dat_LPUEHPDAS_S$LPUE_HPDAS),]

data_wide_LPUEHPDAS <- spread(dat_LPUEHPDAS_S2, Country, LPUE_HPDAS)  # to get wide table
data_wide_LPUEHPDAS <- data_wide_LPUEHPDAS %>%                        # replace country shortcuts by full country names
  rename("Belgium" = "BE",
         "Denmark" = "DK",
         "France" = "FR",
         "Germany" = "GE",
         "Netherlands" = "NL",
         "United_Kingdom" = "UK")
data_wide_sort_LPUEHPDAS <- data_wide_LPUEHPDAS [,c("Year","Germany", "Netherlands", "Denmark", "Belgium", "United_Kingdom", "France")]

write.csv(data_wide_sort_LPUEHPDAS,paste0("WGCRAN",meetingYear,"_Fig.4 Annual LPUE kg per hpDAS ", min(data_wide_sort_LPUEHPDAS$Year),"-", max(data_wide_sort_LPUEHPDAS$Year), "_","update", current.Year, ".csv"), row.names=FALSE)



### LAST 3 YEARS AND 10 YEARS MEAN + SD FOR ALL COUNTRIES: Tables behing Figures 6, 7 and 8 (& Plots) #### 
##### Landings 3 yrs 10 yrs mean. Take care by changing the years! ####
dat_10YearsMean <- dat %>% filter(Year == 2021) %>% group_by(Country, Month) %>% summarise(Landings2021 = Landings)
Landings2022 <- dat %>% filter(Year == 2022) %>% group_by(Country, Month) %>% summarise(Landings = Landings)
dat_10YearsMean$Landings2022 <- Landings2022$Landings
Landings2023 <- dat %>% filter(Year == 2023) %>% group_by(Country, Month) %>% summarise(Landings = Landings)
dat_10YearsMean$Landings2023 <- Landings2023$Landings
MeanLandings <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(Landings = mean(Landings))
dat_10YearsMean$MeanLandings10Years <- MeanLandings$Landings
SDLandings <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(SD = sd(Landings))
dat_10YearsMean$SDLandings10Years <- SDLandings$SD

write.csv(dat_10YearsMean, paste0("WGCRAN",meetingYear,"_Fig.6 Monthly Landings 10 Year Mean SD all countries_update", current.Year,".csv"), row.names=FALSE)

# Plot per country Example for GERMANY
#dat_10YearsMean %>% filter(Country == "GE") %>% ggplot() +
#  geom_line(aes(x = Month, y = Landings2019), linetype = 2) +
#  geom_line(aes(x = Month, y = Landings2020), linetype = 2) +
#  geom_line(aes(x = Month, y = Landings2021)) +
#  geom_errorbar(aes(x = Month, 
#                    ymin = MeanLandings10Years - SDLandings10Years, 
#                    ymax = MeanLandings10Years + SDLandings10Years),
#                width = 0) +
#  geom_line(aes(x = Month, y = MeanLandings10Years), lwd = 1.5) +
#  geom_point(aes(x = Month, y = Landings2019)) +
#  geom_point(aes(x = Month, y = Landings2020)) +
#  geom_point(aes(x = Month, y = Landings2021), size = 3, pch = 22, fill = "red") +
#  ylab("Landings in tonnes") +
#  xlab("Month of the year") +
#  scale_x_continuous(breaks = c(1:12)) +
#  theme_classic()

# All countries combined in one plot
#dat_10YearsMeanPanel <- dat_10YearsMean
#levels(dat_10YearsMeanPanel$Country) <- c("Belgium", "Denmark", "France", "Germany", "Netherlands", "United Kingdom")
#dat_10YearsMeanPanel %>% 
#  mutate(Country = fct_relevel(Country, "Germany", "Netherlands", "Denmark", "Belgium", "United Kingdom", "France")) %>% 
#  ggplot() +
#  geom_line(aes(x = Month, y = Landings2019), linetype = 2) +
#  geom_line(aes(x = Month, y = Landings2020), linetype = 2) +
#  geom_line(aes(x = Month, y = Landings2021)) +
#  geom_errorbar(aes(x = Month, 
#                    ymin = MeanLandings10Years - SDLandings10Years, 
#                    ymax = MeanLandings10Years + SDLandings10Years),
#                width = 0) +
#  geom_line(aes(x = Month, y = MeanLandings10Years), lwd = 1.5) +
#  geom_point(aes(x = Month, y = Landings2019)) +
#  geom_point(aes(x = Month, y = Landings2020)) +
#  geom_point(aes(x = Month, y = Landings2021), size = 3, pch = 22, fill = "red") +
#  ylab("Landings in tonnes") +
#  xlab("Month of the year") +
#  scale_x_continuous(breaks = c(1:12)) +
#  theme(strip.background = element_rect(color = "white", fill = "white"),
#        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#        panel.background = element_blank(), axis.line = element_line(colour = "black"),
#        strip.text = element_text(face = "bold")) +
#  facet_wrap(~ Country , scales = "free")
############LEGEND STILL MISSING

##### Effort in hp days at sea 3 yrs 10 yrs mean. Take care by changing the years! ####
dat_Effort_HPDAYS10YearsMean <- dat %>% filter(Year == 2021) %>% group_by(Country, Month) %>% summarise(Effort_HPDAYS2021 = Effort_HPDAYS)
Effort_HPDAYS2022 <- dat %>% filter(Year == 2022) %>% group_by(Country, Month) %>% summarise(Effort_HPDAYS = Effort_HPDAYS)
dat_Effort_HPDAYS10YearsMean$Effort_HPDAYS2022 <- Effort_HPDAYS2022$Effort_HPDAYS
Effort_HPDAYS2023 <- dat %>% filter(Year == 2023) %>% group_by(Country, Month) %>% summarise(Effort_HPDAYS = Effort_HPDAYS)
dat_Effort_HPDAYS10YearsMean$Effort_HPDAYS2023 <- Effort_HPDAYS2023$Effort_HPDAYS
MeanEffort_HPDAYS <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(Effort_HPDAYS = mean(Effort_HPDAYS))
dat_Effort_HPDAYS10YearsMean$MeanEffort_HPDAYS10Years <- MeanEffort_HPDAYS$Effort_HPDAYS
SDEffort_HPDAYS <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(SD = sd(Effort_HPDAYS))
dat_Effort_HPDAYS10YearsMean$SDEffort_HPDAYS10Years <- SDEffort_HPDAYS$SD

write.csv(dat_Effort_HPDAYS10YearsMean, paste0("WGCRAN",meetingYear,"_Fig.7 Monthly hp_DAS 10 Year Mean SD all countries_update", current.Year,".csv"), row.names=FALSE)

##### LPUE kg HP days at Sea 3 yrs 10 yrs mean. Take care by changing the years!  ####
dat_LPUE_HPDAS10YearsMean <- dat %>% filter(Year == 2021) %>% group_by(Country, Month) %>% summarise(LPUE_HPDAS2021 = LPUE_HPDAS)
LPUE_HPDAS2022 <- dat %>% filter(Year == 2022) %>% group_by(Country, Month) %>% summarise(LPUE_HPDAS = LPUE_HPDAS)
dat_LPUE_HPDAS10YearsMean$LPUE_HPDAS2022 <- LPUE_HPDAS2022$LPUE_HPDAS
LPUE_HPDAS2023 <- dat %>% filter(Year == 2023) %>% group_by(Country, Month) %>% summarise(LPUE_HPDAS = LPUE_HPDAS)
dat_LPUE_HPDAS10YearsMean$LPUE_HPDAS2023 <- LPUE_HPDAS2023$LPUE_HPDAS
MeanLPUE_HPDAS <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(LPUE_HPDAS = mean(LPUE_HPDAS))
dat_LPUE_HPDAS10YearsMean$MeanLPUE_HPDAS10Years <- MeanLPUE_HPDAS$LPUE_HPDAS
SDLPUE_HPDAS <- dat %>% filter(Year %in% c(2014:2023)) %>% group_by(Country, Month) %>% summarise(SD = sd(LPUE_HPDAS))
dat_LPUE_HPDAS10YearsMean$SDLPUE_HPDAS10Years <- SDLPUE_HPDAS$SD

write.csv(dat_LPUE_HPDAS10YearsMean, paste0("WGCRAN",meetingYear,"_Fig.8 Monthly LPUE hp_DAS 10 Year Mean SD all countries_update", current.Year,".csv"), row.names=FALSE)



####################################### by MIRIAM

#### MONTHLY Data in Tables ####

##### Landings monthly ####
dat_sub_land<-dat[,c("Year","Month","Country","Landings")]              # extract columns of interest
data_wide_land <- spread(dat_sub_land, Month, Landings)                 # from "long format" to "wide format"
data_wide_land <-data_wide_land[with(data_wide_land, order(Country)),]  # order by country

write.csv(data_wide_land, paste0("WGCRAN",meetingYear,"_Monthly Landings ",dataYear,"_update", current.Year,".csv"), row.names=FALSE)

##### DAS Days at Sea monthly ####
dat_sub_DAS<-dat[,c("Year","Month","Country","Effort_DAS")]
data_wide_DAS <- spread(dat_sub_DAS, Month, Effort_DAS)                 # from "long format" to "wide format"
data_wide_DAS <-data_wide_DAS[with(data_wide_DAS, order(Country)),]     # order by country
data_wide_DAS2<-na.omit(data_wide_DAS)                                  # omit colums with NAs

write.csv(data_wide_DAS2,paste0("WGCRAN",meetingYear,"_Monthly DAS ",dataYear,"_update", current.Year,".csv"), row.names=FALSE)

##### HPDAS Horse Power Days At Sea monthly ####
dat_sub_HPDAYS<-dat[,c("Year","Month","Country","Effort_HPDAYS")] 
data_wide_HPDAYS <- spread(dat_sub_HPDAYS, Month, Effort_HPDAYS)               # from "long format" to "wide format"
data_wide_HPDAYS <-data_wide_HPDAYS[with(data_wide_HPDAYS, order(Country)),]   # order by country
data_wide_HPDAYS2<-na.omit(data_wide_HPDAYS)                                   # omit colums with NAs

write.csv(data_wide_HPDAYS2,paste0("WGCRAN",meetingYear,"_Monthly hp_DAS ",dataYear,"_update", current.Year,".csv"), row.names=FALSE)

##### LPUE kg per Effort = DAS ####
dat_sub_LPUE_DAS<-dat[,c("Year","Month","Country","LPUE_DAS")] 
data_wide_LPUE_DAS <- spread(dat_sub_LPUE_DAS, Month, LPUE_DAS)                       # from "long format" to "wide format"
data_wide_LPUE_DAS <-data_wide_LPUE_DAS[with(data_wide_LPUE_DAS, order(Country)),]    # order by country
data_wide_LPUE_DAS2<-na.omit(data_wide_LPUE_DAS)                                      # omit colums with NAs

write.csv(data_wide_LPUE_DAS2,paste0("WGCRAN",meetingYear,"_Monthly LPUE DAS ",dataYear,"_update", current.Year,".csv"), row.names=FALSE)

##### LPUE kg per Effort in HPDAS ####
dat_sub_LPUE_HPDAS<-dat[,c("Year","Month","Country","LPUE_HPDAS")]                           
data_wide_LPUE_HPDAS <- spread(dat_sub_LPUE_HPDAS, Month, LPUE_HPDAS)                        # from "long format" to "wide format"
data_wide_LPUE_HPDAS <-data_wide_LPUE_HPDAS[with(data_wide_LPUE_HPDAS, order(Country)),]     # order by country
data_wide_LPUE_HPDAS2<-na.omit(data_wide_LPUE_HPDAS)                                         # omit colums with NAs

write.csv(data_wide_LPUE_HPDAS2,paste0("WGCRAN",meetingYear,"_Monthly LPUE hp_DAS ",dataYear,"_update", current.Year,".csv"), row.names=FALSE)

