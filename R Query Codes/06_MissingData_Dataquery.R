#*QUERY #6 -- CHECK FOR MISSING DATA AND FORMS 
#* Written by: Precious Williams

#* Date Started:21 June 2023
#* Last Updated:16 August 2023

# load packages 

library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(data.table)

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
UploadDate = "2023-08-25"
site = "Kenya"

#*****************************************************************************
#* load data
#*****************************************************************************
## Set working directory to site-specific folder -- main folder
setwd(paste0("~/PRiSMAv2Data/", site, "/", UploadDate, "/", sep = ""))

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 

#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID

# Check for different variations of "N/A" in the MOMID column
na_variations <- c("n/a", "NA", "N/A", "na", NA, "")


if (sum(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID), na.rm = TRUE) > 10) {
  # If more than ten MOMID values match the variations of "N/A",
  # perform the merge by SCRNID.
  mnh01 <- merge(mnh01[, !names(mnh01) %in% c("MOMID", "PREGID")],
                 mnh02[, c("SCRNID", "MOMID", "PREGID")], by = "SCRNID", all.x = TRUE)
} else {
  # If not, print a message indicating that MOMID and PREGID are present.
  print("MOMID and PREGID are present.")
}

# Filter all participants that met the inclusion criteria in MNH process M02 data
Eligible <- mnh02 %>% filter(MOMID != "n/a" &  CONSENT_IEORRES == 1 & AGE_IEORRES == 1 & PC_IEORRES == 1 & 
                               CATCHMENT_IEORRES == 1 & CATCH_REMAIN_IEORRES == 1 & CONSENT_IEORRES == 1) %>% 
  mutate (ENROLLED = TRUE) %>%
  select(MOMID, PREGID) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)


#Parse MNH01 dates to one date format
mnh01$US_OHOSTDAT <- ymd(parse_date_time(mnh01$US_OHOSTDAT,c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

mnh01$US_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

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
         US_GA_DAYS_AGE_FTS4 = ifelse(US_GA_DAYS_AGE_FTS4 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS4)
  ) %>%  
  
  #making the variables numeric
  mutate (US_GA_WKS_AGE_FTS1 = as.numeric(US_GA_WKS_AGE_FTS1),
          US_GA_DAYS_AGE_FTS1 = as.numeric(US_GA_DAYS_AGE_FTS1),
          US_GA_WKS_AGE_FTS2 = as.numeric(US_GA_WKS_AGE_FTS2),
          US_GA_DAYS_AGE_FTS2 = as.numeric(US_GA_DAYS_AGE_FTS2),
          US_GA_WKS_AGE_FTS3 = as.numeric(US_GA_WKS_AGE_FTS3),
          US_GA_DAYS_AGE_FTS3= as.numeric(US_GA_DAYS_AGE_FTS3),
          US_GA_WKS_AGE_FTS4 = as.numeric(US_GA_WKS_AGE_FTS4),
          US_GA_DAYS_AGE_FTS4 = as.numeric(US_GA_DAYS_AGE_FTS4)) %>%
  
  mutate (US_GA_DAYS_AGE_FTS1 = ifelse(is.na(US_GA_DAYS_AGE_FTS1), 0, US_GA_DAYS_AGE_FTS1), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS2 = ifelse(is.na(US_GA_DAYS_AGE_FTS2), 0, US_GA_DAYS_AGE_FTS2), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS3 = ifelse(is.na(US_GA_DAYS_AGE_FTS3), 0, US_GA_DAYS_AGE_FTS3), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS4 = ifelse(is.na(US_GA_DAYS_AGE_FTS4), 0, US_GA_DAYS_AGE_FTS4), #we want to make days 0 if it is empty 
          
          GA_DAYS1 = (US_GA_WKS_AGE_FTS1 * 7) + US_GA_DAYS_AGE_FTS1, #Calculating gestational age in days 
          GA_DAYS2 = (US_GA_WKS_AGE_FTS2 * 7) + US_GA_DAYS_AGE_FTS2, #Calculating gestational age in days  
          GA_DAYS3 = (US_GA_WKS_AGE_FTS3 * 7) + US_GA_DAYS_AGE_FTS3, #Calculating gestational age in days  
          GA_DAYS4 = (US_GA_WKS_AGE_FTS4 * 7) + US_GA_DAYS_AGE_FTS4, #Calculating gestational age in days 
  )%>% 
  mutate(GA_DAYS = pmax(GA_DAYS1, GA_DAYS2, GA_DAYS3, GA_DAYS4,  na.rm = TRUE))

#Choosing the earliest EDD Date, for multiple pregnancies
mnh01 <- mnh01 %>% mutate(US_EDD_BRTHDAT_FTS1 = replace(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS1== ymd("1907-07-07"), NA), 
                          US_EDD_BRTHDAT_FTS2 = replace(US_EDD_BRTHDAT_FTS2, US_EDD_BRTHDAT_FTS2==ymd("1907-07-07"), NA),
                          US_EDD_BRTHDAT_FTS3 = replace(US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS3== ymd("1907-07-07"), NA), 
                          US_EDD_BRTHDAT_FTS4 = replace(US_EDD_BRTHDAT_FTS4, US_EDD_BRTHDAT_FTS4==ymd("1907-07-07"), NA)) %>% 
  mutate(EDD = pmin(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS2, 
                    US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS4,  na.rm = TRUE))

mnh01$EDD <- ymd(parse_date_time(mnh01$EDD, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
#Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)

EDD <- mnh01 %>%
  filter(TYPE_VISIT == 1) %>% #Use only the enrollment visit
  mutate(
    UPLOADDT = as.Date(UploadDate),
    EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
    EDD_EST = EST_CONC_DATE + days(280), 
    DIFF = as.numeric(difftime(EDD, EDD_EST, units = "days"))
  ) %>%
  select(
    MOMID,
    SCRNID,
    PREGID,
    VisitDate = US_OHOSTDAT,
    EDD,
    UPLOADDT,
    EDD_EST,
    GA_DAYS,
    EST_CONC_DATE, 
    DIFF
  ) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175 )

# Create date bounds to get a lower bound and upper bound visit time period
EDD_Date <- EDD %>%
  mutate(
    LW20 = EST_CONC_DATE + days(126), 
    LW28 = EST_CONC_DATE + days(182),
    LW32 = EST_CONC_DATE + days(217),
    LW36 = EST_CONC_DATE + days(238),
    UP20 = EST_CONC_DATE + days(181), 
    UP28 = EST_CONC_DATE + days(216), 
    UP32 = EST_CONC_DATE + days(244), 
    UP36 = EST_CONC_DATE + days(300) 
  ) %>% 
  select(MOMID, PREGID, SCRNID, VisitDate, UPLOADDT, EDD, 
         EDD_EST, GA_DAYS, DIFF, EST_CONC_DATE,
         LW20, LW28, LW32, LW36, UP20, UP28, UP32, UP36) %>%  
  mutate(MOMID = ifelse(MOMID %in% na_variations, NA, MOMID),
         PREGID = ifelse(PREGID %in% na_variations, NA, PREGID)) %>%
  filter(complete.cases(MOMID, PREGID)) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)


#Create a due dates for visit 20, 28, 32 and 36 timeline based on the calculations 0 - not due, 1 - due period, 2 - past due
EDD_Date$DUE20 <- ifelse(EDD_Date$UPLOADDT < EDD_Date$LW20 | EDD_Date$GA_DAYS >= 126, 0,
                         ifelse(EDD_Date$UPLOADDT >= EDD_Date$LW20 & EDD_Date$UPLOADDT <= EDD_Date$UP20 & EDD_Date$GA_DAYS <= 125, 1, 
                                ifelse(EDD_Date$UPLOADDT > EDD_Date$UP20 & EDD_Date$GA_DAYS <= 125, 2, NA))) 

EDD_Date$DUE28 <- ifelse(EDD_Date$UPLOADDT >= EDD_Date$LW28 & EDD_Date$UPLOADDT <= EDD_Date$UP28, 1,
                         ifelse(EDD_Date$UPLOADDT < EDD_Date$LW28, 0, 
                                ifelse(EDD_Date$UPLOADDT > EDD_Date$UP28, 2, NA)))

EDD_Date$DUE32 <- ifelse(EDD_Date$UPLOADDT >= EDD_Date$LW32 & EDD_Date$UPLOADDT <= EDD_Date$UP32, 1,
                         ifelse(EDD_Date$UPLOADDT < EDD_Date$LW32, 0,
                                ifelse(EDD_Date$UPLOADDT > EDD_Date$UP32, 2, NA)))

EDD_Date$DUE36 <- ifelse(EDD_Date$UPLOADDT >= EDD_Date$LW36 & EDD_Date$UPLOADDT <= EDD_Date$UP36, 1,
                         ifelse(EDD_Date$UPLOADDT < EDD_Date$LW36, 0,
                                ifelse(EDD_Date$UPLOADDT > EDD_Date$UP36, 2, NA)))

# Bind the enrolled and screened with the dates by MOMID
Masterlist <- EDD_Date %>% right_join(Eligible, by = c("MOMID", "PREGID")) %>%
  select(MOMID, PREGID, SCRNID, VisitDate, UPLOADDT, EDD, EDD_EST, GA_DAYS, DUE20, DUE28, DUE32, DUE36) %>%   
  mutate(MOMID = ifelse(MOMID %in% na_variations, NA, MOMID),
         PREGID = ifelse(PREGID %in% na_variations, NA, PREGID)) %>%
  filter(complete.cases(EDD, MOMID, PREGID)) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#We want to create an EDD Query where the difference should be less than 14/-14
EDD_query <- EDD_Date %>% right_join(Eligible, by = c("MOMID","PREGID")) %>% 
  mutate (Significant = ifelse(DIFF > 14 | (DIFF < -14), "TRUE", "FALSE"), #if its greater than 7 days then there is a significant difference
          Invalid = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "TRUE", "FALSE")) %>% 
  filter(Significant == "TRUE" | Invalid == "TRUE") %>% 
  mutate(
    EditType = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "Missing US EDD at Enrolment",
                      ifelse((VisitDate == EDD), "Visit Date is the same as EDD",
                             ifelse((DIFF > 7 | (DIFF < -7)) & (!(VisitDate >= UploadDate)), "Inaccurate Ultrasound EDD",   
                                    ifelse((VisitDate >= UploadDate), "Inaccurate Visit Date","Error in US EDD")))),
    UploadDate = UPLOADDT, 
    ScrnID = SCRNID,
    Form = "MNH01",
    `Variable Name` = "US_EDD_BRTHDAT_FTS1",
    `Variable Value` = EDD,
    VisitType = "1",
    InfantID = "NA",
    FieldType = "Date", 
    MomID = MOMID,
    PregID = PREGID,
    DateEditReported = format(Sys.time(), "%Y-%m-%d"),
    Form_Edit_Type = paste(Form,"_",EditType),
    QueryID = paste0(Form, "_", VisitDate, "_", MomID, "_", Form) 
  ) %>%
  select ( QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
           'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)

EDD_query <- EDD_query %>%
  mutate_all(as.character)

Facility <- mnh01  %>%  filter(TYPE_VISIT == 1) %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE)  %>% 
  rename(MomID = MOMID, PregID = PREGID, Location = US_OHOLOC, Facility =US_FAC_SPFY_OHOLOC, 
         Personell = FORMCOMPLID_MNH01 ) %>% 
  select (MomID, PregID, Location, Facility, Personell)

EDD_query_comments <- EDD %>%
  rename(MomID = MOMID, PregID = PREGID, Visitdate = VisitDate, ) %>%
  right_join(EDD_query, by = c("MomID", "PregID")) %>%
  left_join(Facility,  by = c("MomID", "PregID")) %>%
  filter(!(EditType == "Missing US EDD at Enrolment")) %>% 
  select(QueryID, ScrnID, MomID, PregID, UploadDate, VisitDate, Site_EDD = EDD,
         'Expected_EDD' = EDD_EST, Difference = DIFF, `Gestational Age in Days` = GA_DAYS, EditType,Location, Facility, Personell)

EDD_query_comments <- EDD_query_comments %>%
  mutate_all(as.character)

if (nrow(EDD_query_comments) >= 1) {
  save(EDD_query_comments, file = "queries/EDD_query_comments.rda")
} 

# Save the EDD_query dataframe as an .rda file
if (nrow(EDD_query) >= 1) {
  save(EDD_query, file = "queries/EDD_query.rda")
} 

#Begin query for MNH01 to find missing
## Because there are some enrolled individuals that are missing mnh01 forms
Miss_enrol_01 <- mnh01 %>%
  filter(TYPE_VISIT == 1) %>% right_join(Eligible, by = c("MOMID","PREGID")) %>%
  mutate(
    Query =   ifelse(MAT_VISIT_MNH01 %in% c(1, 2), FALSE,
                     ifelse(MAT_VISIT_MNH01 %in% c(77, 55,99, 88), "Invalid Data Entry for Enrolment Visit",   
                            ifelse(MAT_VISIT_MNH01 %in% c(3,4,5,6), "Enrolment Visit was not done", "Missing MNH01 Enrolment Form"))),
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01", 
    Varname = "MAT_VISIT_MNH01", 
    VisitDate = US_OHOSTDAT,
    VisitType = "1", 
    UPLOADDT = UploadDate) %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH01 - ANC 32
Missing_01_32 <- mnh01 %>% 
  filter(TYPE_VISIT == 4) %>% select (-c ("EDD")) %>% 
  right_join (Masterlist, by = c("MOMID","PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH01 %in% c(1, 2), FALSE,
                   ifelse(DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH01 %in% c(77, 55,99, 88) & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32",   
                                 ifelse(MAT_VISIT_MNH01 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH01) & DUE32 == 2, "Missing Visit 32 Ultrasound Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01",
    VisitDate = US_OHOSTDAT,
    VisitType = "4", 
    UPLOADDT = UploadDate,
    Varname = "MAT_VISIT_MNH01") %>%
  filter(!(Query %in% c(FALSE, "Not Due"))) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH02 - ENROLMENT 

Missing_02 <- EDD_Date %>% 
  left_join(mnh02, by = c("MOMID", "PREGID"))  %>% 
  right_join(mnh03,  by = c("MOMID", "PREGID"))  %>%
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
Missing_03 <- Eligible %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  left_join(mnh03,  by = c("MOMID", "PREGID"))  %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse(MAT_VISIT_MNH03 %in% c(1, 2), FALSE,
                        ifelse(MAT_VISIT_MNH03 %in% c (3, 4, 5, 6, 77, 55,99, 88, -7, -5, -9), "Invalid Data Entry for Enrolment Visit", 
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
Miss_enrol_04 <- mnh04 %>% filter( TYPE_VISIT == 1) %>% 
  right_join(Eligible,  by = c("MOMID", "PREGID")) %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH04 %in% c(1, 2)), FALSE,
                        ifelse(MAT_VISIT_MNH04 %in% c(3, 4, 5, 6, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH04 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH04,
         Form = "MNH04",
         VisitDate = ANC_OBSSTDAT,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH04") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 20 (For those who need to have ANC20 done)
Missing_04_20 <- mnh04 %>%
  filter(TYPE_VISIT == 2) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2), FALSE,
                   ifelse(DUE20 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH04 %in% c(77, 55,99, 88, ".") & DUE20 %in% c(1, 2), "Invalid Data Entry for Visit 20", 
                                 ifelse(MAT_VISIT_MNH04 %in% c(3,4,5,6)  & DUE20  %in% c(1, 2), "Visit 20 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH04) & DUE20 == 2, "Missing MNH04 Visit 20 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 28
Missing_04_28 <-  mnh04 %>%
  filter(TYPE_VISIT == 3) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH04) & DUE28 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH04 %in% c(77, 55,99, 88, ".") & DUE28 %in% c(1, 2), "Invalid Data Entry for Visit 28", 
                                 ifelse(MAT_VISIT_MNH04 %in% c(3,4,5,6)  & DUE28  %in% c(1, 2), "Visit 28 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH04) & DUE28 == 2, "Missing MNH04 Visit 28 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "3",
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 32
Missing_04_32 <- mnh04 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH04) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH04 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH04 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH04) & DUE32 == 2, "Missing MNH04 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 36
Missing_04_36 <-mnh04 %>%
  filter(TYPE_VISIT == 5) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH04) & DUE36 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH04 %in% c(77, 55,99, 88, ".") & DUE36 %in% c(1, 2), "Invalid Data Entry for Visit 36", 
                                 ifelse(MAT_VISIT_MNH04 %in% c(3,4,5,6)  & DUE36  %in% c(1, 2), "Visit 36 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH04) & DUE36 == 2, "Missing MNH04 Visit 36 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "5",
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH05 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH05 - ENROLMENT
Miss_enrol_05 <- mnh05 %>% filter( TYPE_VISIT == 1) %>% 
  right_join(Eligible,  by = c("MOMID", "PREGID")) %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH05 %in% c(1, 2)), FALSE,
                        ifelse(MAT_VISIT_MNH05 %in% c(3, 4, 5, 6, 77, 55,99, 88, -7, -9, -5, "."), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH05 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH05,
         Form = "MNH05",
         VisitDate = ANT_PEDAT,
         VisitType = "1",
         Varname = "MAT_VISIT_MNH05") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 20 (For those who need to have ANC20 done)
Missing_05_20 <- mnh05 %>%
  filter(TYPE_VISIT == 2) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2),  FALSE,
                   ifelse(is.na(MAT_VISIT_MNH05) & DUE20 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH05 %in% c(77, 55,99, 88, ".") & DUE20 %in% c(1, 2), "Invalid Data Entry for Visit 20", 
                                 ifelse(MAT_VISIT_MNH05 %in% c(3,4,5,6)  & DUE20  %in% c(1, 2), "Visit 20 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH05) & DUE20 == 2, "Missing MNH05 Visit 20 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH05,
    VisitDate = ANT_PEDAT,
    VisitType = "2",
    Form = "MNH05",
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 28
Missing_05_28 <-  mnh05 %>%
  filter(TYPE_VISIT == 3) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH05) & DUE28 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH05 %in% c(77, 55,99, 88, ".") & DUE28 %in% c(1, 2), "Invalid Data Entry for Visit 28", 
                                 ifelse(MAT_VISIT_MNH05 %in% c(3,4,5,6)  & DUE28  %in% c(1, 2), "Visit 28 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH05) & DUE28 == 2, "Missing MNH05 Visit 28 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "3",
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 32
Missing_05_32 <- mnh05 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH05) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH05 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH05 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH05) & DUE32 == 2, "Missing MNH05 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH05,
    VisitDate = ANT_PEDAT,
    VisitType = "4",
    Form = "MNH05",
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 36
Missing_05_36 <-mnh05 %>%
  filter(TYPE_VISIT == 5) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH05) & DUE36 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH05 %in% c(77, 55,99, 88, ".") & DUE36 %in% c(1, 2), "Invalid Data Entry for Visit 36", 
                                 ifelse(MAT_VISIT_MNH05 %in% c(3,4,5,6)  & DUE36  %in% c(1, 2), "Visit 36 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH05) & DUE36 == 2, "Missing MNH05 Visit 36 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "5",
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH06 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36
#ENROLMENT

Miss_enrol_06 <- mnh06 %>% filter( TYPE_VISIT == 1) %>% 
  right_join(Eligible,  by = c("MOMID", "PREGID")) %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH06 %in% c(1, 2)), FALSE,
                        ifelse(MAT_VISIT_MNH06 %in% c(3, 4, 5, 6, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH06 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH06,
         Form = "MNH06",
         VisitDate = DIAG_VSDAT,
         VisitType = "1",
         Varname = "MAT_VISIT_MNH06") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 20 (For those who need to have ANC20 done)
Missing_06_20 <- mnh06 %>%
  filter(TYPE_VISIT == 2) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2),  FALSE,
                   ifelse(is.na(MAT_VISIT_MNH06) & DUE20 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH06 %in% c(77, 55,99, 88, ".") & DUE20 %in% c(1, 2), "Invalid Data Entry for Visit 20", 
                                 ifelse(MAT_VISIT_MNH06 %in% c(3,4,5,6)  & DUE20  %in% c(1, 2), "Visit 20 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH06) & DUE20 == 2, "Missing MNH06 Visit 20 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "2",
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 28
Missing_06_28 <-  mnh06 %>%
  filter(TYPE_VISIT == 3) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH06) & DUE28 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH06 %in% c(77, 55,99, 88, ".") & DUE28 %in% c(1, 2), "Invalid Data Entry for Visit 28", 
                                 ifelse(MAT_VISIT_MNH06 %in% c(3,4,5,6)  & DUE28  %in% c(1, 2), "Visit 28 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH06) & DUE28 == 2, "Missing MNH06 Visit 28 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "3",
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 32
Missing_06_32 <- mnh06 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH06) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH06 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH06 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH06) & DUE32 == 2, "Missing MNH06 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 36
Missing_06_36 <-mnh06 %>%
  filter(TYPE_VISIT == 5) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH06) & DUE36 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH06 %in% c(77, 55,99, 88, ".") & DUE36 %in% c(1, 2), "Invalid Data Entry for Visit 36", 
                                 ifelse(MAT_VISIT_MNH06 %in% c(3,4,5,6)  & DUE36  %in% c(1, 2), "Visit 36 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH06) & DUE36 == 2, "Missing MNH06 Visit 36 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "5",
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)



#MNH07 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#ENROLMENT
#MNH07 - ENROLMENT
Miss_enrol_07 <- mnh07 %>% filter( TYPE_VISIT == 1) %>% 
  right_join(Eligible,  by = c("MOMID", "PREGID")) %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH07 %in% c(1, 2)), FALSE,
                        ifelse(MAT_VISIT_MNH07 %in% c(3, 4, 5, 6, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH07 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH07,
         Form = "MNH07",
         VisitDate = MAT_SPEC_COLLECT_DAT,
         VisitType = "1",
         Varname = "MAT_VISIT_MNH07") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 20 (For those who need to have ANC20 done)
Missing_07_20 <- mnh07 %>%
  filter(TYPE_VISIT == 2) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2),  FALSE,
                   ifelse(is.na(MAT_VISIT_MNH07) & DUE20 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH07 %in% c(77, 55,99, 88, ".") & DUE20 %in% c(1, 2), "Invalid Data Entry for Visit 20", 
                                 ifelse(MAT_VISIT_MNH07 %in% c(3,4,5,6)  & DUE20  %in% c(1, 2), "Visit 20 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH07) & DUE20 == 2, "Missing MNH07 Visit 20 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "2",
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 28
Missing_07_28 <-  mnh07 %>%
  filter(TYPE_VISIT == 3) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH07) & DUE28 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH07 %in% c(77, 55,99, 88, ".") & DUE28 %in% c(1, 2), "Invalid Data Entry for Visit 28", 
                                 ifelse(MAT_VISIT_MNH07 %in% c(3,4,5,6)  & DUE28  %in% c(1, 2), "Visit 28 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH07) & DUE28 == 2, "Missing MNH07 Visit 28 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "3",
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 32
Missing_07_32 <- mnh07 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH07) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH07 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH07 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH07) & DUE32 == 2, "Missing MNH07 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 36
Missing_07_36 <-mnh07 %>%
  filter(TYPE_VISIT == 5) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH07) & DUE36 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH07 %in% c(77, 55,99, 88, ".") & DUE36 %in% c(1, 2), "Invalid Data Entry for Visit 36", 
                                 ifelse(MAT_VISIT_MNH07 %in% c(3,4,5,6)  & DUE36  %in% c(1, 2), "Visit 36 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH07) & DUE36 == 2, "Missing MNH07 Visit 36 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "5",
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH08 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#ENROLMENT
#MNH08 - ENROLMENT
Miss_enrol_08 <- mnh08 %>% filter( TYPE_VISIT == 1) %>% 
  right_join(Eligible,  by = c("MOMID", "PREGID")) %>% 
  left_join(Masterlist, by = c("MOMID", "PREGID"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH08 %in% c(1, 2)), FALSE,
                        ifelse(MAT_VISIT_MNH08 %in% c(3, 4, 5, 6, 77, 55,99, 88, -7, -9, -5), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH08 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH08,
         VisitDate = LBSTDAT,
         VisitType = "1",
         Form = "MNH08",
         Varname = "MAT_VISIT_MNH08") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 20 (For those who need to have ANC20 done)
Missing_08_20 <- mnh08 %>%
  filter(TYPE_VISIT == 2) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2),  FALSE,
                   ifelse(is.na(MAT_VISIT_MNH08) & DUE20 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH08 %in% c(77, 55,99, 88, ".") & DUE20 %in% c(1, 2), "Invalid Data Entry for Visit 20", 
                                 ifelse(MAT_VISIT_MNH08 %in% c(3,4,5,6)  & DUE20  %in% c(1, 2), "Visit 20 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH08) & DUE20 == 2, "Missing MNH08 Visit 20 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "2",
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 28
Missing_08_28 <-  mnh08 %>%
  filter(TYPE_VISIT == 3) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH08) & DUE28 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH08 %in% c(77, 55,99, 88, ".") & DUE28 %in% c(1, 2), "Invalid Data Entry for Visit 28", 
                                 ifelse(MAT_VISIT_MNH08 %in% c(3,4,5,6)  & DUE28  %in% c(1, 2), "Visit 28 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH08) & DUE28 == 2, "Missing MNH08 Visit 28 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "3",
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 32
Missing_08_32 <- mnh08 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH08) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH08 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH08 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH08) & DUE32 == 2, "Missing MNH08 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 36
Missing_08_36 <-mnh08 %>%
  filter(TYPE_VISIT == 5) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2),  FALSE,
                   ifelse(is.na(MAT_VISIT_MNH08) & DUE36 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH08 %in% c(77, 55,99, 88, ".") & DUE36 %in% c(1, 2), "Invalid Data Entry for Visit 36", 
                                 ifelse(MAT_VISIT_MNH08 %in% c(3,4,5,6)  & DUE36  %in% c(1, 2), "Visit 36 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH08) & DUE36 == 2, "Missing MNH08 Visit 36 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "5",
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)



#MNH25 - ANC32 
#Because Kenya does not have Type_Visit

if (exists("mnh25")) {
  if ("TYPE_VISIT" %in% colnames(mnh25)) {
    print("TYPE_VISIT exists.")
  } else {
    mnh25 <- mnh25 %>% mutate(
      TYPE_VISIT = ANC_VISIT_N)
    print("TYPE_VISIT column created with ANC_VISIT_N values.")}
}

Missing_25_32 <- mnh25 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH25 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH25) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH25 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH25 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH25) & DUE32 == 2, "Missing MNH25 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH25,
    Form = "MNH25",
    VisitDate = OBSSTDAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH25") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query) %>% mutate_all(as.character)

#MNH26 - ANC32 
#Because Kenya does not have Type_Visit

if (exists("mnh26")) {
  if ("TYPE_VISIT" %in% colnames(mnh26)) {
    print("TYPE_VISIT exists.")
  } else {
    mnh26 <- mnh26 %>% mutate(
      TYPE_VISIT = 4)
    print("TYPE_VISIT column created")}
}
Missing_26_32 <- mnh26 %>%
  filter(TYPE_VISIT == 4) %>% 
  right_join(Masterlist, by = c("MOMID", "PREGID")) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH26 %in% c(1, 2), FALSE,
                   ifelse(is.na(MAT_VISIT_MNH26) & DUE32 %in% c(0, 1), "Not Due",
                          ifelse(MAT_VISIT_MNH26 %in% c(77, 55,99, 88, ".") & DUE32 %in% c(1, 2), "Invalid Data Entry for Visit 32", 
                                 ifelse(MAT_VISIT_MNH26 %in% c(3,4,5,6)  & DUE32  %in% c(1, 2), "Visit 32 was not done", 
                                        ifelse(is.na(MAT_VISIT_MNH26) & DUE32 == 2, "Missing MNH26 Visit 32 Form", "Data Entry Error"))))),
    Variable_Value = MAT_VISIT_MNH26,
    Form = "MNH26",
    VisitDate = FTGE_OBSTDAT,
    VisitType = "4",
    Varname = "MAT_VISIT_MNH26") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)%>% mutate_all(as.character)

# Get all delivered moms that have had a preterm delivery in Visit 20, 28 and 32 & also a delivery from and remove the MOMIDS & PREGIDs from missing across forms
if (site != "Kenya") {
  
  mnh09 <- mnh09 %>%
    mutate(MAT_LD_OHOSTDAT = ymd(parse_date_time(MAT_LD_OHOSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
           # Replace specific dates with NA in MAT_LD_OHOSTDAT
           MAT_LD_OHOSTDAT = replace(MAT_LD_OHOSTDAT, MAT_LD_OHOSTDAT == ymd("1907-07-07"), NA),
           MAT_LD_OHOSTDAT = replace(MAT_LD_OHOSTDAT, MAT_LD_OHOSTDAT == ymd("1707-07-07"), NA)
    ) %>%
    # Filter out rows where MAT_LD_OHOSTDAT is NA
    filter(!is.na(MAT_LD_OHOSTDAT))
  
  delivered_moms <- mnh09 %>% left_join(EDD_Date) %>% # Left join mnh09 with EDD_Date based on a common key/column
    # Calculate the difference in days between MAT_LD_OHOSTDAT and EDD_EST + # Calculate gestational age at delivery (GA_DELIV)
    mutate(GA_DELIV = 280 -(as.integer(difftime(EDD_EST, MAT_LD_OHOSTDAT, units = "days"))),
           GA_DELIV2 = GA_DAYS + (as.integer(difftime(MAT_LD_OHOSTDAT, VisitDate,  units = "days"))),
           Delivered = 1,
           Preterm = ifelse(GA_DELIV < 180, "Yes_20",
                            ifelse(GA_DELIV >= 181 & GA_DELIV <= 216, "Yes_28",  
                                   ifelse(GA_DELIV >= 217 & GA_DELIV <= 237, "Yes_32",
                                          "No"))))%>%
    select(MOMID, PREGID,GA_DELIV,GA_DELIV2, Preterm, Delivered, GA_DAYS, VisitDate, MAT_LD_OHOSTDAT)
  
  preterm_20_ <- delivered_moms %>%  filter(Preterm == "Yes_20") %>% select(MOMID, PREGID)
  preterm_28_ <- delivered_moms %>%  filter(Preterm %in% c("Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  preterm_32_ <- delivered_moms %>%  filter(Preterm %in% c("Yes_20", "Yes_28","Yes_32")) %>% select(MOMID, PREGID)
  delivered_36_  <- delivered_moms %>%  filter(Delivered == 1) %>% select(MOMID, PREGID)
  
  
  # List of dataframes ending in 20, 28, 32, 36 
  dataframes_20 <- ls(pattern = "_20$")
  dataframes_28 <- ls(pattern = "_28$")
  dataframes_32 <- ls(pattern = "_32$")
  dataframes_36 <- ls(pattern = "_36$")
  
  # Loop through each dataframe ending in "_20" and apply the filtering
  for (df_name in dataframes_20) {
    if ("MOMID" %in% colnames(get(df_name)) & "PREGID" %in% colnames(get(df_name))) {
      assign(df_name, anti_join(get(df_name), preterm_20_, by = c("MOMID", "PREGID")))}
  }
  
  # Loop through each dataframe ending in "_28" and apply the filtering
  for (df_name in dataframes_28) {
    if ("MOMID" %in% colnames(get(df_name)) & "PREGID" %in% colnames(get(df_name))) {
      assign(df_name, anti_join(get(df_name), preterm_28_, by = c("MOMID", "PREGID")))}
  }
  
  # Loop through each dataframe ending in "_32" and apply the filtering
  for (df_name in dataframes_32) {
    if ("MOMID" %in% colnames(get(df_name)) & "PREGID" %in% colnames(get(df_name))) {
      assign(df_name, anti_join(get(df_name), preterm_32_, by = c("MOMID", "PREGID")))}
  }
  
  # Loop through each dataframe ending in "_36" and apply the filtering
  for (df_name in dataframes_36) {
    if ("MOMID" %in% colnames(get(df_name)) & "PREGID" %in% colnames(get(df_name))) {
      assign(df_name, anti_join(get(df_name), delivered_36_, by = c("MOMID", "PREGID")))}
  }
}

#Bind all dataframes by forms 
Missing_01 <- rbind (Missing_01_32, Miss_enrol_01)  %>% mutate_all(as.character)
Missing_04 <- rbind (Miss_enrol_04, Missing_04_20, Missing_04_28, Missing_04_32, Missing_04_36) %>% mutate_all(as.character)
Missing_05 <- rbind (Miss_enrol_05, Missing_05_20, Missing_05_28, Missing_05_32, Missing_05_36) %>% mutate_all(as.character)
Missing_06 <- rbind (Miss_enrol_06, Missing_06_20, Missing_06_28, Missing_06_32, Missing_06_36) %>% mutate_all(as.character)
Missing_07 <- rbind (Miss_enrol_07, Missing_07_20, Missing_07_28, Missing_07_32, Missing_07_36) %>% mutate_all(as.character)
Missing_08 <- rbind (Miss_enrol_08, Missing_08_20, Missing_08_28, Missing_08_32, Missing_08_36) %>% mutate_all(as.character)
Missing_25 <- Missing_25_32 %>% mutate_all(as.character)
Missing_26 <- Missing_26_32 %>% mutate_all(as.character)

#Bind all Missing Forms

MissingData_query <- rbindlist(list(Missing_01, Missing_02, Missing_03, Missing_04, Missing_05, 
                                    Missing_06, Missing_07, Missing_08, Missing_25, Missing_26))  %>% 
  mutate(UploadDate = UploadDate, 
         ScrnID = "NA",
         `Variable Name` = Varname,
         `Variable Value` = Variable_Value,
         InfantID = "NA",
         FieldType = "NA",  
         EditType = Query,
         MomID = MOMID,
         PregID = PREGID,
         DateEditReported = format(Sys.time(), "%Y-%m-%d"),
         Form_Edit_Type = paste(Form,"_",EditType),
         QueryID = paste0(Form, "_",UploadDate, "_",MomID, "_", Form) 
  )  %>%
  select ( QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
           'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)


save(MissingData_query, file = "queries/MissingData_query.rda")                
