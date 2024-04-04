#*QUERY #6 -- CHECK FOR MISSING VISITS AND FORMS 
#* Written by: Precious Williams

#* Date Started: 21 June 2023
#* Last Updated: 14 March 2024
#* Updated queries to only identify missing forms
#* Updated queries to compile all visits into one line of query
#* developed a comments rda file

rm(list = ls())

# load packages 

library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(data.table)
library(lubridate)

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
UploadDate = "2024-03-15"
site = "Pakistan"

#*****************************************************************************
#* load data
#*****************************************************************************
## Set working directory to site-specific folder -- main folder
setwd(paste0("~/PRiSMAv2Data/", site, "/", UploadDate, "/", sep = ""))

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 

#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID

#Check for different variations of "N/A" in the MOMID column

if (site == "Ghana") {
  
  mnh02_id <- mnh02 %>% select (MOMID, PREGID, SCRNID)
  #mnh01 <- merge(mnh01, mnh02_id, by = c ("MOMID", "PREGID"))
  
} else {
  
  na_variations <- c("n/a", "NA", "N/A", "na", NA, "")
  
  if (sum(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID), na.rm = TRUE) > 2) {
    # If more than ten MOMID values match the variations of "N/A",
    # perform the merge by SCRNID.
    mnh01 <- merge(mnh01[, !names(mnh01) %in% c("MOMID", "PREGID")],
                   mnh02[, c("SCRNID", "MOMID", "PREGID")], by = "SCRNID", all.x = TRUE)
  } else {
    # If not, print a message indicating that MOMID and PREGID are present.
    
    print("MOMID and PREGID are present.")
  }
  
  mnh01 <- mnh01[!(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID)), ]
  
}

#Parse MNH01 dates to one date format
mnh01$US_OHOSTDAT <- ymd(parse_date_time(mnh01$US_OHOSTDAT,c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$ESTIMATED_EDD_SCDAT <- ymd(parse_date_time(mnh01$ESTIMATED_EDD_SCDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

# mnh01$US_OHOSTDAT <- ymd(parse_date_time(mnh01$US_OHOSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

#Calculating GA and choosing the "latest" possible GA 

mnh01 <- mnh01 %>% 
  #making default values equal to NA
  mutate(US_GA_WKS_AGE_FTS1 = ifelse(US_GA_WKS_AGE_FTS1 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS1),
         US_GA_DAYS_AGE_FTS1 = ifelse(US_GA_DAYS_AGE_FTS1 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS1),
         US_GA_WKS_AGE_FTS2 = ifelse(US_GA_WKS_AGE_FTS2 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS2),
         US_GA_DAYS_AGE_FTS2 = ifelse(US_GA_DAYS_AGE_FTS2 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS2),
         US_GA_WKS_AGE_FTS3 = ifelse(US_GA_WKS_AGE_FTS3 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS3),
         US_GA_DAYS_AGE_FTS3 = ifelse(US_GA_DAYS_AGE_FTS3 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS3),
         US_GA_WKS_AGE_FTS4 = ifelse(US_GA_WKS_AGE_FTS4 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS4),
         US_GA_DAYS_AGE_FTS4 = ifelse(US_GA_DAYS_AGE_FTS4 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS4),
         GA_LMP_WEEKS_SCORRES = ifelse(GA_LMP_WEEKS_SCORRES %in% c(-7, 77), NA, GA_LMP_WEEKS_SCORRES),
         GA_LMP_DAYS_SCORRES = ifelse(GA_LMP_DAYS_SCORRES %in% c(-7, 77), NA, GA_LMP_DAYS_SCORRES)
         
  ) %>%  
  
  #making the variables numeric
  mutate (US_GA_WKS_AGE_FTS1 = as.numeric(US_GA_WKS_AGE_FTS1),
          US_GA_DAYS_AGE_FTS1 = as.numeric(US_GA_DAYS_AGE_FTS1),
          US_GA_WKS_AGE_FTS2 = as.numeric(US_GA_WKS_AGE_FTS2),
          US_GA_DAYS_AGE_FTS2 = as.numeric(US_GA_DAYS_AGE_FTS2),
          US_GA_WKS_AGE_FTS3 = as.numeric(US_GA_WKS_AGE_FTS3),
          US_GA_DAYS_AGE_FTS3= as.numeric(US_GA_DAYS_AGE_FTS3),
          US_GA_WKS_AGE_FTS4 = as.numeric(US_GA_WKS_AGE_FTS4),
          US_GA_DAYS_AGE_FTS4 = as.numeric(US_GA_DAYS_AGE_FTS4),
          GA_LMP_WEEKS_SCORRES = as.numeric(GA_LMP_WEEKS_SCORRES),
          GA_LMP_DAYS_SCORRES = as.numeric(GA_LMP_DAYS_SCORRES)) %>%
  
  mutate (US_GA_DAYS_AGE_FTS1 = ifelse(is.na(US_GA_DAYS_AGE_FTS1), 0, US_GA_DAYS_AGE_FTS1), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS2 = ifelse(is.na(US_GA_DAYS_AGE_FTS2), 0, US_GA_DAYS_AGE_FTS2), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS3 = ifelse(is.na(US_GA_DAYS_AGE_FTS3), 0, US_GA_DAYS_AGE_FTS3), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS4 = ifelse(is.na(US_GA_DAYS_AGE_FTS4), 0, US_GA_DAYS_AGE_FTS4), #we want to make days 0 if it is empty 
          GA_LMP_WEEKS_SCORRES = ifelse(is.na(GA_LMP_WEEKS_SCORRES), 0, GA_LMP_WEEKS_SCORRES),
          GA_LMP_DAYS_SCORRES = ifelse(is.na(GA_LMP_DAYS_SCORRES), 0, GA_LMP_DAYS_SCORRES),
          
          GA_DAYS1 = (US_GA_WKS_AGE_FTS1 * 7) + US_GA_DAYS_AGE_FTS1, #Calculating gestational age in days 
          GA_DAYS2 = (US_GA_WKS_AGE_FTS2 * 7) + US_GA_DAYS_AGE_FTS2, #Calculating gestational age in days  
          GA_DAYS3 = (US_GA_WKS_AGE_FTS3 * 7) + US_GA_DAYS_AGE_FTS3, #Calculating gestational age in days  
          GA_DAYS4 = (US_GA_WKS_AGE_FTS4 * 7) + US_GA_DAYS_AGE_FTS4, #Calculating gestational age in days 
          GA_LMP = (GA_LMP_WEEKS_SCORRES * 7) + GA_LMP_DAYS_SCORRES #Calculating gestational age by LMP in days 
          
  )%>% 
  mutate(GA_DAYS = pmax(GA_DAYS1, GA_DAYS2, GA_DAYS3, GA_DAYS4,  na.rm = TRUE))

#Choosing the earliest EDD Date, for multiple pregnancies
mnh01 <- mnh01 %>% mutate(US_EDD_BRTHDAT_FTS1 = replace(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS1== ymd("1907-07-07") | US_EDD_BRTHDAT_FTS1== ymd("2007-07-07") , NA), 
                          US_EDD_BRTHDAT_FTS2 = replace(US_EDD_BRTHDAT_FTS2, US_EDD_BRTHDAT_FTS2==ymd("1907-07-07") | US_EDD_BRTHDAT_FTS2== ymd("2007-07-07"), NA),
                          US_EDD_BRTHDAT_FTS3 = replace(US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS3== ymd("1907-07-07")| US_EDD_BRTHDAT_FTS3== ymd("2007-07-07"), NA), 
                          US_EDD_BRTHDAT_FTS4 = replace(US_EDD_BRTHDAT_FTS4, US_EDD_BRTHDAT_FTS4==ymd("1907-07-07")| US_EDD_BRTHDAT_FTS4== ymd("2007-07-07"), NA),
                          ESTIMATED_EDD_SCDAT = replace(ESTIMATED_EDD_SCDAT, ESTIMATED_EDD_SCDAT==ymd("1907-07-07")| ESTIMATED_EDD_SCDAT== ymd("2007-07-07"), NA)) %>% 
  mutate(EDD = pmin(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS2, 
                    US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS4,  na.rm = TRUE))

mnh01$EDD <- ymd(parse_date_time(mnh01$EDD, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))


na_variations <- c("n/a", "NA", "N/A", "na", NA, "")

# Filter all participants that met the inclusion criteria in MNH process M02 data
Eligible <- mnh02 %>% filter (SCRN_RETURN == 1 & CONSENT_IEORRES == 1 & AGE_IEORRES == 1  & PC_IEORRES == 1  & CATCHMENT_IEORRES == 1  & CATCH_REMAIN_IEORRES == 1) %>%
  mutate (Eligible = 1) %>% select(SCRNID, Eligible, SCRN_RETURN)  %>%
  distinct(SCRNID, .keep_all = TRUE)

#Step 0.5a:Developing a Masterlist of Enrolled Individuals
Screened <- mnh01 %>% filter (TYPE_VISIT == 1 & GA_DAYS <= 146 & GA_DAYS != 0 & MAT_VISIT_MNH01 %in% c(1,2) ) %>% 
  select (MOMID, PREGID, SCRNID, US_OHOSTDAT, EDD, GA_DAYS, TYPE_VISIT, ESTIMATED_EDD_SCDAT,GA_LMP)  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) 

Enroled <- merge(Screened, Eligible, by = c("SCRNID"))  %>% select (-c("TYPE_VISIT"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)
EDD <- Enroled %>%
  mutate(
    UPLOADDT = as.Date(UploadDate),
    EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
    EDD_EST = EST_CONC_DATE + days(280), 
    DIFF = abs(as.numeric(difftime(EDD, EDD_EST, units = "days"))),
    DIFF_GA = abs(as.numeric(GA_LMP - GA_DAYS)),
    DIFF_EDD = abs(as.numeric(difftime(ESTIMATED_EDD_SCDAT, EDD, units = "days")))) %>%
  select( MOMID, SCRNID, PREGID, VisitDate = US_OHOSTDAT, ESTIMATED_EDD_SCDAT, EDD,
          UPLOADDT, EDD_EST, GA_DAYS, GA_LMP, EST_CONC_DATE, DIFF, DIFF_GA,DIFF_EDD) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175)

Enrolled <- Enroled %>% mutate(GA_TODAY = round(GA_DAYS + as.numeric(difftime(UploadDate, US_OHOSTDAT, units = "days"))))  %>%
  filter (GA_TODAY >= 139) 
#**************************************************************************************************************
#PART B: MISSING FORM QUERY

if (site=="India_CMC"){
  
  CAL_GA <- mnh01 %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(CAL_GA_WKS_AGE_FTS1!= -7 & CAL_GA_DAYS_AGE_FTS1 != -7,  (CAL_GA_WKS_AGE_FTS1 * 7 + CAL_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(CAL_GA_WKS_AGE_FTS2!= -7 & CAL_GA_DAYS_AGE_FTS2 != -7,  (CAL_GA_WKS_AGE_FTS2 * 7 + CAL_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(CAL_GA_WKS_AGE_FTS3!= -7 & CAL_GA_DAYS_AGE_FTS3 != -7,  (CAL_GA_WKS_AGE_FTS3 * 7 + CAL_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(CAL_GA_WKS_AGE_FTS4!= -7 & CAL_GA_DAYS_AGE_FTS4 != -7,  (CAL_GA_WKS_AGE_FTS4 * 7 + CAL_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>%
    mutate(CAL_EDD_BRTHDAT_FTS1 = replace(CAL_EDD_BRTHDAT_FTS1, CAL_EDD_BRTHDAT_FTS1== ymd("1907-07-07") | CAL_EDD_BRTHDAT_FTS1== ymd("2007-07-07") , NA), 
           CAL_EDD_BRTHDAT_FTS2 = replace(CAL_EDD_BRTHDAT_FTS2, CAL_EDD_BRTHDAT_FTS2==ymd("1907-07-07") | CAL_EDD_BRTHDAT_FTS2== ymd("2007-07-07"), NA),
           CAL_EDD_BRTHDAT_FTS3 = replace(CAL_EDD_BRTHDAT_FTS3, CAL_EDD_BRTHDAT_FTS3== ymd("1907-07-07")| CAL_EDD_BRTHDAT_FTS3== ymd("2007-07-07"), NA), 
           CAL_EDD_BRTHDAT_FTS4 = replace(CAL_EDD_BRTHDAT_FTS4, CAL_EDD_BRTHDAT_FTS4==ymd("1907-07-07")| CAL_EDD_BRTHDAT_FTS4== ymd("2007-07-07"), NA)) %>%
    mutate(EDD = pmin(CAL_EDD_BRTHDAT_FTS1, CAL_EDD_BRTHDAT_FTS2, 
                      CAL_EDD_BRTHDAT_FTS3, CAL_EDD_BRTHDAT_FTS4,  na.rm = TRUE)) %>%
    select(MOMID, PREGID, GA_DAYS, EDD) %>% filter (!is.na(EDD))
  
  EDD <- merge (EDD[, !names(EDD) %in% c("EDD", "GA_DAYS")],
                CAL_GA, by = c("MOMID", "PREGID"), all.x = TRUE)
  
}

# Create date bounds to get a lower bound and upper bound visit time period
EDD_Date <- EDD %>%
  mutate(
    LW20 = EST_CONC_DATE + days(160), 
    LW28 = EST_CONC_DATE + days(216),
    LW32 = EST_CONC_DATE + days(237),
    LW36 = EST_CONC_DATE + days(272),
    UP20 = EST_CONC_DATE + days(181), 
    UP28 = EST_CONC_DATE + days(216), 
    UP32 = EST_CONC_DATE + days(237), 
    UP36 = EST_CONC_DATE + days(272)) %>% 
  select(MOMID, PREGID, VisitDate, UPLOADDT, EDD, 
         EDD_EST, GA_DAYS, DIFF, EST_CONC_DATE, ESTIMATED_EDD_SCDAT,DIFF_GA, GA_LMP,
         DIFF_EDD,
         LW20, LW28, LW32, LW36, UP20, UP28, UP32, UP36) %>%  
  mutate(MOMID = ifelse(MOMID %in% na_variations, NA, MOMID),
         PREGID = ifelse(PREGID %in% na_variations, NA, PREGID)) %>%
  filter(complete.cases(MOMID, PREGID)) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#Create a due dates for ANC20, 3, 4, 5 timeline based on the calculations FALSE - not due, TRUE - due period, 2 - past due
EDD_Date$DUE20 <- ifelse ((EDD_Date$UPLOADDT > EDD_Date$UP20) & (EDD_Date$GA_DAYS <= 125), TRUE, FALSE) 
EDD_Date$DUE28 <- ifelse (EDD_Date$UPLOADDT > EDD_Date$UP28, TRUE, FALSE)
EDD_Date$DUE32 <- ifelse (EDD_Date$UPLOADDT > EDD_Date$UP32, TRUE, FALSE)
EDD_Date$DUE36 <- ifelse (EDD_Date$UPLOADDT > EDD_Date$UP36, TRUE, FALSE)


if (exists("mnh09")==TRUE){
  
  mnh09$MAT_LD_OHOSTDAT <- ymd(parse_date_time(mnh09$MAT_LD_OHOSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  
  delivered_moms <- mnh09 %>% left_join(EDD, by = c("MOMID", "PREGID")) %>% # Left join mnh09 with EDD_Date based on a common key/column
    # Calculate the difference in days between MAT_LD_OHOSTDAT and EDD_EST + # Calculate gestational age at delivery (GA_DELIV)
    mutate(GA_DELIV = 280 - (as.integer(difftime(EDD, MAT_LD_OHOSTDAT, units = "days"))),
           Delivered = 1,
           Premies = ifelse(GA_DELIV < 181, "Yes_20",
                            ifelse(GA_DELIV >= 182 & GA_DELIV <= 216, "Yes_28",  
                                   ifelse(GA_DELIV >= 217 & GA_DELIV <= 237, "Yes_32",
                                          ifelse(GA_DELIV >= 238 & GA_DELIV <= 272, "Yes_36", "No"))))) %>%
    select(MOMID, PREGID, GA_DELIV, Premies, Delivered, GA_DAYS, VisitDate, MAT_LD_OHOSTDAT)
  
  premies_20_ <- delivered_moms %>%  filter(Premies == "Yes_20") %>% select(MOMID, PREGID)
  premies_28_ <- delivered_moms %>%  filter(Premies %in% c("Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  premies_32_ <- delivered_moms %>%  filter(Premies %in% c("Yes_20", "Yes_28","Yes_32")) %>% select(MOMID, PREGID)
  premies_36_  <- delivered_moms %>%  filter(Premies %in% c("Yes_20", "Yes_28","Yes_32")) %>% select(MOMID, PREGID)
  miss_36_d <- delivered_moms %>%  filter(Premies %in% c("Yes_36")) %>% mutate (Delivered = 1) %>% select(MOMID, PREGID, Delivered)
  
}

if (exists("mnh23")==TRUE){
  
  mnh23$CLOSE_DSSTDAT <- ymd(parse_date_time(mnh23$CLOSE_DSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  
  censored_moms <- mnh23 %>%
    left_join(EDD, by = c("MOMID", "PREGID")) %>%
    mutate(
      GA_CEN = GA_DAYS + (CLOSE_DSSTDAT - VisitDate),
      Censored = 1,
      Cen_Time = ifelse(GA_CEN > 300, "No",
                        ifelse(GA_CEN < 181, "Yes_20",
                               ifelse(GA_CEN >= 182 & GA_CEN <= 216, "Yes_28",
                                      ifelse(GA_CEN >= 217 & GA_CEN <= 237, "Yes_32", 
                                             ifelse(GA_CEN >= 238 & GA_CEN <= 272, "Yes_36", "Remove_All")))))) %>%
    select(MOMID, PREGID, GA_CEN, Censored, Cen_Time, GA_DAYS, VisitDate, CLOSE_DSSTDAT)
  
  cens_20_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All", "Yes_20")) %>% select(MOMID, PREGID)
  cens_28_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  cens_32_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32")) %>% select(MOMID, PREGID)
  cens_36_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32", "Yes_36")) %>% select(MOMID, PREGID)
  
}

if (exists("mnh04")==TRUE) {
  
  mnh04$ANC_OBSSTDAT <- ymd(parse_date_time(mnh04$ANC_OBSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  
  miscarriage <- mnh04 %>%
    filter(if_any(FETAL_LOSS_DSDECOD, ~ . %in% c(1, 2, 3))) %>% 
    mutate(Visit_Date = as.Date(coalesce(!!!select(., "ANC_OBSSTDAT"))))  %>%  # Move as.Date outside mutate
    select(MOMID, PREGID, Visit_Date) %>% 
    left_join(EDD, by = c("MOMID", "PREGID")) %>% 
    mutate(GA_Visit = GA_DAYS + (Visit_Date - VisitDate),  # Use consistent column name Visit_Date
           Miss_Time = ifelse(GA_Visit < 181, "Yes_20",
                              ifelse(GA_Visit >= 182 & GA_Visit <= 216, "Yes_28",
                                     ifelse(GA_Visit >= 217 & GA_Visit <= 237, "Yes_32", 
                                            ifelse(GA_Visit >= 238 & GA_Visit <= 272, "Yes_36", "Remove_All")))))
  
  miss_20_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20")) %>% select(MOMID, PREGID)
  miss_28_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  miss_32_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32")) %>% select(MOMID, PREGID)
  miss_36_  <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20","Yes_28", "Yes_32",  "Yes_36")) %>% select(MOMID, PREGID)
  
}


# Create data frames for different due times
due20_df <- EDD_Date %>%
  select (-c("DUE28", "DUE32", "DUE36")) %>%
  filter (DUE20 == TRUE)  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_20_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_20_, by = c("MOMID", "PREGID")) %>%
  anti_join(miss_20_, by = c("MOMID", "PREGID"))

due28_df <- EDD_Date %>%
  select (-c("DUE20", "DUE32", "DUE36")) %>%
  filter (DUE28 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_28_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_28_, by = c("MOMID", "PREGID"))  %>%
  anti_join(miss_28_, by = c("MOMID", "PREGID"))

due32_df <- EDD_Date %>%
  select (-c("DUE20", "DUE28", "DUE36")) %>%
  filter (DUE32 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_32_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_32_, by = c("MOMID", "PREGID"))  %>%
  anti_join(miss_32_, by = c("MOMID", "PREGID")) 

due36_df <- EDD_Date %>%
  select (-c("DUE20", "DUE28", "DUE32")) %>%
  filter (DUE36 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(premies_36_, by = c("MOMID", "PREGID")) %>%
  anti_join(cens_36_, by = c("MOMID", "PREGID"))  %>% 
  anti_join(miss_36_d, by = c("MOMID", "PREGID")) %>%
  anti_join(miss_36_, by = c("MOMID", "PREGID"))   

#Begin query for MNH01 to find missing
## Because there are some enrolled individuals that are missing mnh01 forms

Enrol_01 <- mnh01  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH01) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_01 <- Enrolled %>% left_join (Enrol_01, by = c("MOMID", "PREGID")) %>% 
  mutate(
    Query = ifelse(MAT_VISIT_MNH01 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(MAT_VISIT_MNH01 %in% c(77, 55, 99), "Invalid Data Entry for Enrolment Visit")),  
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01", 
    Varname = "MAT_VISIT_MNH01", 
    VisitDate = US_OHOSTDAT,
    VisitType = "1", 
    UPLOADDT = UploadDate) %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH01 - ANC 32
ANC32_01 <- mnh01  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH01, US_OHOSTDAT ) 

Missing_01_32 <- due32_df %>% left_join (ANC32_01, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH01 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH01 %in% c(77, 55, 99)) & (DUE32 == TRUE), "Invalid Data Entry for ANC32 MNH01 Form",   
                                 ifelse(is.na(MAT_VISIT_MNH01) & DUE32 == TRUE, "Missing ANC32 MNH01 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01",
    VisitDate = US_OHOSTDAT,
    VisitType = "4", 
    UPLOADDT = UploadDate,
    Varname = "MAT_VISIT_MNH01") %>%
  filter(!(Query %in% c(FALSE, "Not Due", "Not Applicable"))) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH02 - ENROLMENT 
Missing_02 <- EDD_Date %>% 
  left_join(mnh02, by = c("MOMID", "PREGID"))  %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse (CONSENT_IEORRES == 1, FALSE,
                    ifelse (!(CONSENT_IEORRES %in% c(1,0)), "Invalid Data Entry for Enrolment Visit","Missing MNH02 Enrolment Form")),
    Variable_Value = CONSENT_IEORRES,
    Form = "MNH02",
    VisitDate = SCRN_OBSSTDAT,
    VisitType = "1", 
    UPLOADDT = UploadDate,
    Varname = "CONSENT_IEORRES"
  ) %>%
  filter(Query != FALSE | (is.na(MOMID) & is.na(PREGID))) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query) %>% mutate_all(as.character)


#MNH03 - ENROLMENT
#There should only be one form per participant, hence the distinct MOMID code 
#We want to identify forms that are not present, or have a default value in the MAT_Visit Variable
Missing_03 <- Enrolled %>% 
  left_join(mnh03,  by = c("MOMID", "PREGID"))  %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse(MAT_VISIT_MNH03 %in% c(1, 2, 3,4,5, 6, 7, 8), FALSE,
                        ifelse(MAT_VISIT_MNH03 %in% c (3, 4, 5, 77, 55, 99, 88, -7, -5, -9), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH03 Enrolment Form")),
         Form = "MNH03",
         Varname = "MAT_VISIT_MNH03",
         VisitDate = SD_OBSSTDAT,
         VisitType = "1", 
         UPLOADDT = UploadDate,
         Variable_Value = MAT_VISIT_MNH03
  ) %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query) %>% mutate_all(as.character)

#MNH04 
#ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36
#MNH04 - ENROLMENT
Enrol_04 <- mnh04  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH04, ANC_OBSSTDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_04 <- Enrolled %>% left_join (Enrol_04, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH04 %in% c(1, 2, 3,4,5, 6, 7, 8)), FALSE,
                        ifelse(MAT_VISIT_MNH04 %in% c(3, 4, 5, 77, 55, 99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH04 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH04,
         Form = "MNH04",
         VisitDate = ANC_OBSSTDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH04") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 20 (For those who need to have ANC20 done)
ANC20_04 <- mnh04  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH04,ANC_OBSSTDAT ) 
Missing_04_20 <- due20_df %>% left_join (ANC20_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH04 %in% c(77, 55, 99)) & (DUE20 == TRUE), "Invalid Data Entry for MNH04 ANC20",   
                                 ifelse(is.na(MAT_VISIT_MNH04) & DUE20 == TRUE, "Missing ANC20 MNH04 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 28
ANC28_04 <- mnh04  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH04, ANC_OBSSTDAT) 
Missing_04_28 <- due28_df %>% left_join (ANC28_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH04 %in% c(77, 55,99)) & (DUE28 == TRUE), "Invalid Data Entry for MNH04 ANC28",   
                                 ifelse(is.na(MAT_VISIT_MNH04) & DUE28 == TRUE, "Missing ANC28 MNH04 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH04 - ANC 32
ANC32_04 <- mnh04  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH04,ANC_OBSSTDAT ) 
Missing_04_32 <- due32_df %>% left_join (ANC32_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH04 %in% c(77, 55,99)) & (DUE32 == TRUE), "Invalid Data Entry for ANC32",   
                                 ifelse(is.na(MAT_VISIT_MNH04) & DUE32 == TRUE, "Missing ANC32 MNH04 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 36
ANC36_04 <- mnh04  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH04,ANC_OBSSTDAT ) 
Missing_04_36 <- due36_df %>% left_join (ANC36_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH04 %in% c(77, 55,99)) & (DUE36 == TRUE), "Invalid Data Entry for MNH04 ANC36",   
                                 ifelse(is.na(MAT_VISIT_MNH04) & DUE36 == TRUE, "Missing ANC36 MNH04 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH05 - ENROLMENT
Enrol_05 <- mnh05  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH05, ANT_PEDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_05 <- Enrolled %>% left_join (Enrol_05, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH05 %in% c(1, 2, 6, 7, 8)), FALSE,
                        ifelse(MAT_VISIT_MNH05 %in% c(3, 4, 5, 77, 55, 99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH05 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH05,
         Form = "MNH05",
         VisitDate = ANT_PEDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH05") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 20 (For those who need to have ANC20 done)
ANC20_05 <- mnh05  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_20 <- due20_df %>% left_join (ANC20_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH05 %in% c(77, 55, 99)) & (DUE20 == TRUE), "Invalid Data Entry for MNH05 ANC20",   
                                 ifelse(is.na(MAT_VISIT_MNH05) & DUE20 == TRUE, "Missing ANC20 MNH05 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 28
ANC28_05 <- mnh05  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_28 <- due28_df %>% left_join (ANC28_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH05 %in% c(77, 55,99)) & (DUE28 == TRUE), "Invalid Data Entry for MNH05 ANC28",   
                                 ifelse(is.na(MAT_VISIT_MNH05) & DUE28 == TRUE, "Missing ANC28 MNH05 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH05 - ANC 32
ANC32_05 <- mnh05  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_32 <- due32_df %>% left_join (ANC32_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH05 %in% c(77, 55,99)) & (DUE32 == TRUE), "Invalid Data Entry for MNH05 ANC32",   
                                 ifelse(is.na(MAT_VISIT_MNH05) & DUE32 == TRUE, "Missing ANC32 MNH05 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 36
ANC36_05 <- mnh05  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_36 <- due36_df %>% left_join (ANC36_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH05 %in% c(77, 55,99)) & (DUE36 == TRUE), "Invalid Data Entry for MNH05 ANC36",   
                                 ifelse(is.na(MAT_VISIT_MNH05) & DUE36 == TRUE, "Missing ANC36 MNH05  Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH06 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH06 - ENROLMENT
Enrol_06 <- mnh06  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH06, DIAG_VSDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_06 <- Enrolled %>% left_join (Enrol_06, by = c("MOMID", "PREGID")) %>% 
  mutate(Query = ifelse((MAT_VISIT_MNH06 %in% c(1, 2, 3, 4, 5, 6, 7, 8)), FALSE,
                        ifelse(MAT_VISIT_MNH06 %in% c(3, 4, 5, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH06 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH06,
         Form = "MNH06",
         VisitDate = DIAG_VSDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH06") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 20 (For those who need to have ANC20 done)
ANC20_06 <- mnh06  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_20 <- due20_df %>% left_join (ANC20_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH06 %in% c(77, 55,99)) & (DUE20 == TRUE), "Invalid Data Entry for MNH06 ANC20",   
                                 ifelse(is.na(MAT_VISIT_MNH06) & DUE20 == TRUE, "Missing ANC20 MNH06 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 28
ANC28_06 <- mnh06  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_28 <- due28_df %>% left_join (ANC28_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH06 %in% c(77, 55,99)) & (DUE28 == TRUE), "Invalid Data Entry for MNH06 ANC28 ",   
                                 ifelse(is.na(MAT_VISIT_MNH06) & DUE28 == TRUE, "Missing ANC28 MNH06 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH06 - ANC 32
ANC32_06 <- mnh06  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_32 <- due32_df %>% left_join (ANC32_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH06 %in% c(77, 55,99)) & (DUE32 == TRUE), "Invalid Data Entry for MNH06 ANC32",   
                                 ifelse(is.na(MAT_VISIT_MNH06) & DUE32 == TRUE, "Missing ANC32 MNH06 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 36
ANC36_06 <- mnh06  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_36 <- due36_df %>% left_join (ANC36_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH06 %in% c(77, 55,99)) & (DUE36 == TRUE), "Invalid Data Entry for MNH06 ANC36",   
                                 ifelse(is.na(MAT_VISIT_MNH06) & DUE36 == TRUE, "Missing ANC36 MNH06 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH07 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH07 ENROLMENT
Enrol_07 <- mnh07  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH07, MAT_SPEC_COLLECT_DAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_07 <- Enrolled %>% left_join (Enrol_07, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8)), FALSE,
                        ifelse(MAT_VISIT_MNH07 %in% c(3, 4, 5, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH07 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH07,
         Form = "MNH07",
         VisitDate = MAT_SPEC_COLLECT_DAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH07") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 20 (For those who need to have ANC20 done)
ANC20_07 <- mnh07  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_20 <- due20_df %>% left_join (ANC20_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH07 %in% c(77, 55,99)) & (DUE20 == TRUE), "Invalid Data Entry for MNH07 ANC20",   
                                 ifelse(is.na(MAT_VISIT_MNH07) & DUE20 == TRUE, "Missing ANC20 MNH07 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 28
ANC28_07 <- mnh07  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_28 <- due28_df %>% left_join (ANC28_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH07 %in% c(77, 55,99)) & (DUE28 == TRUE), "Invalid Data Entry for ANC28  MNH07 Form",   
                                 ifelse(is.na(MAT_VISIT_MNH07) & DUE28 == TRUE, "Missing ANC28 MNH07 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH07 - ANC 32
ANC32_07 <- mnh07  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_32 <- due32_df %>% left_join (ANC32_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH07 %in% c(77, 55,99)) & (DUE32 == TRUE), "Invalid Data Entry for ANC32  MNH07 Form",   
                                 ifelse(is.na(MAT_VISIT_MNH07) & DUE32 == TRUE, "Missing ANC32 MNH07 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 36
ANC36_07 <- mnh07  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_36 <- due36_df %>% left_join (ANC36_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH07 %in% c(77, 55,99)) & (DUE36 == TRUE), "Invalid Data Entry for ANC36  MNH07 Form",   
                                 ifelse(is.na(MAT_VISIT_MNH07) & DUE36 == TRUE, "Missing ANC36 MNH07 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH08 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36
#MNH08 - ENROLMENT
Enrol_08 <- mnh08  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH08, LBSTDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_08 <- Enrolled %>% left_join (Enrol_08, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8)), FALSE,
                        ifelse(MAT_VISIT_MNH08 %in% c(3, 4, 5, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH08 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH08,
         Form = "MNH08",
         VisitDate = LBSTDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH08") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 20 (For those who need to have ANC20 done)
ANC20_08 <- mnh08  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_20 <- due20_df %>% left_join (ANC20_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH08 %in% c(77, 55,99)) & (DUE20 == TRUE), "Invalid Data Entry for ANC20",   
                                 ifelse(is.na(MAT_VISIT_MNH08) & DUE20 == TRUE, "Missing ANC20 MNH08 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 28
ANC28_08 <- mnh08  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_28 <- due28_df %>% left_join (ANC28_08, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH08 %in% c(77, 55,99)) & (DUE28 == TRUE), "Invalid Data Entry for ANC28",   
                                 ifelse(is.na(MAT_VISIT_MNH08) & DUE28 == TRUE, "Missing ANC28 MNH08 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 32
ANC32_08 <- mnh08  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_32 <- due32_df %>% left_join (ANC32_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH08 %in% c(77, 55,99)) & (DUE32 == TRUE), "Invalid Data Entry for ANC32",   
                                 ifelse(is.na(MAT_VISIT_MNH08) & DUE32 == TRUE, "Missing ANC32 MNH08 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 36
ANC36_08 <- mnh08  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_36 <- due36_df %>% left_join (ANC36_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse((MAT_VISIT_MNH08 %in% c(77, 55, 99)) & (DUE36 == TRUE), "Invalid Data Entry for ANC36",   
                                 ifelse(is.na(MAT_VISIT_MNH08) & DUE36 == TRUE, "Missing ANC36 MNH08 Form", "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

# Combine all forms' data
Missing_01 <- rbind (Missing_01_32, Miss_enrol_01)  %>% mutate_all(as.character)
Missing_04 <- rbind (Miss_enrol_04, Missing_04_20, Missing_04_28, Missing_04_32, Missing_04_36) %>% mutate_all(as.character)
Missing_05 <- rbind (Miss_enrol_05, Missing_05_20, Missing_05_28, Missing_05_32, Missing_05_36) %>% mutate_all(as.character)
Missing_06 <- rbind (Miss_enrol_06, Missing_06_20, Missing_06_28, Missing_06_32, Missing_06_36) %>% mutate_all(as.character)
Missing_07 <- rbind (Miss_enrol_07, Missing_07_20, Missing_07_28, Missing_07_32, Missing_07_36) %>% mutate_all(as.character)
Missing_08 <- rbind (Miss_enrol_08, Missing_08_20, Missing_08_28, Missing_08_32, Missing_08_36) %>% mutate_all(as.character)
all_forms_missing <- rbindlist(list(Missing_01, Missing_02, Missing_03, Missing_04, Missing_05, Missing_06, Missing_07, Missing_08)) %>% select(-VisitDate, -EDD, -UPLOADDT)

# Separate missing and invalid queries
missing_data_query <- all_forms_missing %>% filter(is.na(Variable_Value))
invalid_data_entry <- all_forms_missing %>% filter(!is.na(Variable_Value)) %>% 
  select (MOMID, PREGID, VisitType, Form, Varname, Variable_Value, EditType = Query)

missing_forms_category <- missing_data_query %>%
  group_by(MOMID, PREGID, VisitType) %>%
  summarise(Missing_Forms = paste(Form, collapse = ", "))

missing_forms_count <- missing_data_query %>%
  group_by(MOMID, PREGID, VisitType) %>%
  count(name = "Frequency")

missing_forms_merged <- merge(missing_forms_count, missing_forms_category, by = c("VisitType", "MOMID", "PREGID"))

missing_forms_merged$VisitTypeLabel <- case_when(
  missing_forms_merged$VisitType == 2 ~ "ANC20",
  missing_forms_merged$VisitType == 1 ~ "Enrolment",
  missing_forms_merged$VisitType == 3 ~ "ANC28",
  missing_forms_merged$VisitType == 4 ~ "ANC32",
  missing_forms_merged$VisitType == 5 ~ "ANC36"
)
missing_forms_merged <- missing_forms_merged %>%
  mutate(EditType = ifelse(Frequency >= 5, paste0("Missing All ", VisitTypeLabel, " Forms"),
                    ifelse(Frequency < 5 & Frequency > 1, paste0("Missing Multiple", " ", VisitTypeLabel, " Forms"), 
                           paste0("Missing ", VisitTypeLabel, " ",  Missing_Forms, " Form"))),
         Form = Missing_Forms,
         Varname = NA,
        Variable_Value = NA)  

all_missing_label <- missing_forms_merged %>% 
 select (MOMID, PREGID, VisitType, Form, Varname, Variable_Value, EditType)

# Combine all types of missing entries
combined_query <- rbind(all_missing_label, invalid_data_entry)

#Bind all Missing Forms
MissingForms_query <- combined_query  %>% 
  mutate(UploadDate = UploadDate, 
         ScrnID = "NA",
         `Variable Name` = Varname,
         `Variable Value` = Variable_Value,
         InfantID = "NA",
         FieldType = "NA",  
         MomID = MOMID,
         PregID = PREGID,
         VisitDate = "NA",
         DateEditReported = format(Sys.time(), "%Y-%m-%d"),
         Form_Edit_Type = paste(Form,"_", EditType),
         QueryID = paste0(MomID, "_", UploadDate, "_", EditType, "_", "05" ) 
  )  %>%
  select ( QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
           'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)

save(MissingForms_query, file = "queries/MissingForms_query.rda") 
 

#create comments to specify missing forms
missingEDD <- missing_forms_merged %>% left_join(EDD, by = c ("MOMID","PREGID" )) %>% 
                                 select (MOMID, PREGID, GA_DAYS, VisitDate, VisitType, Missing_Forms, EditType )

MissingFormsQuery_comments <- missingEDD %>% 
  mutate( `GA at Upload Date` = round(GA_DAYS + as.numeric(difftime(UploadDate, VisitDate, units = "days"))),
          QueryID = paste0(MOMID, "_", UploadDate, "_", EditType, "_", "05" ))  %>% 
  rename(`GA at Enrolment` = GA_DAYS, `Enrolment Date` = VisitDate, `Missing Forms` = Missing_Forms)

save(MissingFormsQuery_comments, file = "queries/MissingFormsQuery_comments.rda") 
                                
