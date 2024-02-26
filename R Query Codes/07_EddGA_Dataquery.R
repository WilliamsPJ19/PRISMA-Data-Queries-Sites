#*QUERY #5.5 -- CHECK FOR MISSING DATA AND FORMS 
#* Written by: Precious Williams

#* Date Started:21 June 2023
#* Last Updated:29 November 2023
#* Updated queries to only identify missing forms
#* Updated EDD/GA variables for India_CMC
#* Updated EDD Query to include US vs LMP GA & US queries 

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
UploadDate = "2024-02-16"
site = "India-SAS"

#*****************************************************************************
#* load data
#*****************************************************************************
## Set working directory to site-specific folder -- main folder
setwd(paste0("~/PRiSMAv2Data/", site, "/", UploadDate, "/", sep = ""))

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 

#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID

# Check for different variations of "N/A" in the MOMID column

if (site == "Ghana") {
  
  mnh02_id <- mnh02 %>% select (MOMID, PREGID, SCRNID)
  mnh01 <- merge(mnh01, mnh02_id, by = c ("MOMID", "PREGID"))
  
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
Eligible <- mnh02 %>% filter (CONSENT_IEORRES == 1 & AGE_IEORRES == 1  & PC_IEORRES == 1  & CATCHMENT_IEORRES == 1  & CATCH_REMAIN_IEORRES == 1) %>%
  mutate (Eligible = 1) %>% select(SCRNID, Eligible)  %>%
  distinct(SCRNID, .keep_all = TRUE)

#Step 0.5a:Developing a Masterlist of Enrolled Individuals
Screened <- mnh01 %>% filter (TYPE_VISIT == 1 & GA_DAYS <= 146 & GA_DAYS != 0 & MAT_VISIT_MNH01 %in% c(1,2) ) %>% 
  select (MOMID, PREGID, SCRNID, US_OHOSTDAT, EDD, GA_DAYS, TYPE_VISIT, ESTIMATED_EDD_SCDAT,GA_LMP)  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) 

Enrolled <- merge(Screened, Eligible, by = c("SCRNID"))  %>% select (-c("TYPE_VISIT"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)
EDD <- Enrolled %>%
  mutate(
    UPLOADDT = as.Date(UploadDate),
    EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
    EDD_EST = EST_CONC_DATE + days(280), 
    DIFF = abs(as.numeric(difftime(EDD, EDD_EST, units = "days"))),
    DIFF_GA = abs(as.numeric(GA_LMP - GA_DAYS)),
    DIFF_EDD = abs(as.numeric(difftime(ESTIMATED_EDD_SCDAT, EDD, units = "days")))) %>%
  select(
    MOMID,
    SCRNID,
    PREGID,
    VisitDate = US_OHOSTDAT,
    ESTIMATED_EDD_SCDAT,
    EDD,
    UPLOADDT,
    EDD_EST,
    GA_DAYS,
    GA_LMP,
    EST_CONC_DATE, 
    DIFF,
    DIFF_GA,
    DIFF_EDD) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175)

#PART A: EDD QUERY CODE
#We want to create an EDD Query where the difference should be less than 14/-14

EDD_query <- EDD %>% 
  mutate (Significant = ifelse((DIFF > 14 & GA_DAYS != 0) | (DIFF_GA > 50 & GA_LMP != 0 ) | (DIFF_EDD > 50 & ESTIMATED_EDD_SCDAT != 0 & EDD != 0), "TRUE", "FALSE"), #if its greater than 7 days then there is a significant difference
          Invalid = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "TRUE", "FALSE")) %>% 
  filter(Significant == "TRUE" | Invalid == "TRUE") %>% 
  mutate(
    EditType = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "Missing US EDD at Enrolment",
                      ifelse((VisitDate == EDD), "Visit Date is the same as EDD",
                             ifelse((DIFF > 14 | (DIFF < -14)) & (!(VisitDate >= UploadDate)), "Inaccurate Ultrasound EDD",
                                    ifelse((DIFF_GA > 50) & (!(VisitDate >= UploadDate)) & (GA_LMP != 0), "Inconsistencies Between LMP GA and US GA",
                                           ifelse((DIFF_EDD > 50) & (!(VisitDate >= UploadDate)) & (ESTIMATED_EDD_SCDAT != 0), "Inconsistencies Between LMP EDD and US EDD",
                                                  ifelse((VisitDate >= UploadDate), "Inaccurate Visit Date","Error in US EDD"))))))) %>% 
  mutate(
    `Variable Name` = ifelse((EditType == "Inconsistencies Between LMP EDD and US EDD"), "ESTIMATED_EDD_SCDAT",
                             ifelse((EditType == "Inconsistencies Between LMP GA and US GA"), "GA_LMP_WEEKS_SCORRES/GA_LMP_DAYS_SCORRES", "US_EDD_BRTHDAT_FTS1")),
    `Variable Value` = ifelse((EditType == "Inconsistencies Between LMP EDD and US EDD"), (format(ESTIMATED_EDD_SCDAT, "%Y-%m-%d")),
                              ifelse((EditType == "Inconsistencies Between LMP GA and US GA"), GA_LMP, (format(EDD, "%Y-%m-%d")))),
    
    UploadDate = UPLOADDT, 
    ScrnID = SCRNID,
    Form = "MNH01",
    VisitType = "1",
    InfantID = "NA",
    FieldType = "Date", 
    MomID = MOMID,
    PregID = PREGID,
    DateEditReported = format(Sys.time(), "%Y-%m-%d"),
    Form_Edit_Type = paste(Form,"_",EditType),
    QueryID = paste0(Form, "_", VisitDate, "_", MomID, "_", Form) 
  ) %>%
  select (QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
          'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)

EDD_query <- EDD_query %>%
  mutate_all(as.character)

Facility <- mnh01  %>%  filter(TYPE_VISIT == 1) %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE)  %>% 
  rename(MomID = MOMID, PregID = PREGID, Location = US_OHOLOC, Facility =US_FAC_SPFY_OHOLOC, 
         Personell = FORMCOMPLID_MNH01 ) %>% 
  select (MomID, PregID, Location, Facility, Personell)

EDD_query_comments <- EDD %>%
  rename(MomID = MOMID, PregID = PREGID, Visitdate = VisitDate) %>%
  right_join(EDD_query, by = c("MomID", "PregID")) %>%
  left_join(Facility,  by = c("MomID", "PregID")) %>%
  filter(!(EditType == "Missing US EDD at Enrolment")) %>% 
  select(QueryID, MomID, PregID, UploadDate, VisitDate, `Reported Ultrasound EDD` = EDD,
         `Expected EDD` = EDD_EST,   `Difference:Expected-Site_EDD` = DIFF, 
         ESTIMATED_EDD_SCDAT, `US-LMP:EDD_Difference:` = DIFF_EDD,
         `US Gestational Age in Days` = GA_DAYS, `LMP Gestational Age in Days` = GA_LMP, `GA_Differnce: EDD vs LMP` = DIFF_GA,
         EditType, Location, Facility, Personell)

EDD_query_comments <- EDD_query_comments %>%
  mutate_all(as.character)

if (nrow(EDD_query_comments) >= 1) {
  save(EDD_query_comments, file = "queries/EDD_query_comments.rda")
} 

# Save the EDD_query dataframe as an .rda file
if (nrow(EDD_query) >= 1) {
  save(EDD_query, file = "queries/EDD_query.rda")
} 
