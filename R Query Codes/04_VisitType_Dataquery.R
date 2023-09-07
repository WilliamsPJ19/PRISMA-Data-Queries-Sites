#*****************************************************************************
#*QUERY #4 -- CHECK THAT VISIT TYPE AND GA AND/OR PNC DAYS MATCH WITH WHAT IS REPORTED 
#* Written by: Stacie Loisate 
#* Last updated: 06 September 2023

#*Input: Long data 
#*Function: check for visit types that do not match 
#*Output: .rda file with all mismatched visit types 
#*****************************************************************************
#* Data setup 
#*****************************************************************************

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)


# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: Set "site" variable to the site you are running the query for 
site = "Kenya"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

## UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

#*****************************************************************************
#* not all sites are reporting momid and pregid in mnh01 - this will cause some issues in this workflow 
#* solution: merge in momid and pregid from MNH02 into MNH01 based on scrnid
#*****************************************************************************
mnh02_ids <- mnh02 %>% select(SCRNID, MOMID, PREGID)
mnh01_newids <- mnh01 %>% select(-MOMID, -PREGID) %>% ## remove momid and pregid vars in mnh01 - will use ids from mnh02
  left_join(mnh02_ids, by = c("SCRNID"))


#*****************************************************************************
#*Export key variables from each dataframe: MOMID, PREGID, VISIT DATE 
#* for MNH01 and 09: export GA and delivery information 
#*****************************************************************************
mnh01_raw <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT,
                                     US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1, US_GA_WKS_AGE_FTS2, US_GA_DAYS_AGE_FTS2,
                                     US_GA_WKS_AGE_FTS3, US_GA_DAYS_AGE_FTS3, US_GA_WKS_AGE_FTS4, US_GA_DAYS_AGE_FTS4) 
## only extract key variables from each of the forms that check for visit type 
## for MNH01, need to extract the maximum GA age (currently using US -- will eventually update to ACOG)
mnh01_sub <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT,
                                     US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1, US_GA_WKS_AGE_FTS2, US_GA_DAYS_AGE_FTS2,
                                     US_GA_WKS_AGE_FTS3, US_GA_DAYS_AGE_FTS3, US_GA_WKS_AGE_FTS4, US_GA_DAYS_AGE_FTS4) %>% 
  ## extract the maximum gestational age for each woman 
  mutate(GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA)) %>% 
  mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
  ## convert to weeks 
  mutate(GA_US_WKS = floor(GA_US_DAYS/7)) %>% 
  mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  ## remove any unscheduled visit types
  filter(TYPE_VISIT != 13) %>% 
  select(MOMID, PREGID, TYPE_VISIT, US_OHOSTDAT,GA_US_DAYS, GA_US_WKS)

## remove any duplicates -- these will be flagged in the duplicate id code
out_dup_ids <- mnh01_sub[duplicated(mnh01_sub[,1:3]),]
out_dup_ids <- out_dup_ids %>% pull(MOMID)
mnh01_sub <- mnh01_sub %>% filter(!(MOMID %in% out_dup_ids))

## separate into 2 dataframes: one with GA at enrollment visit only -- we will use this to calculate GA at each visit. and one with the other visit types  
mnh01_ga_at_enrl <- mnh01_sub %>% filter(TYPE_VISIT == 1) %>% select(-TYPE_VISIT)
mnh01_sub <- mnh01_sub %>% filter(TYPE_VISIT != 1) %>% rename("VISITDATE" = "US_OHOSTDAT") %>% 
  select(-c(GA_US_DAYS, GA_US_WKS)) %>% 
  mutate(FORM = "MNH01")

## extract key variables from each of the forms that require visit type
mnh04_sub <- mnh04 %>% select(MOMID, PREGID, TYPE_VISIT, ANC_OBSSTDAT) %>% 
  mutate(ANC_OBSSTDAT = ymd(parse_date_time(ANC_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "ANC_OBSSTDAT") %>% 
  mutate(FORM = "MNH04")

mnh05_sub <- mnh05 %>% select(MOMID, PREGID, TYPE_VISIT, ANT_PEDAT) %>%
  mutate(ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "ANT_PEDAT") %>%
  mutate(FORM = "MNH05")

mnh06_sub <- mnh06 %>% select(MOMID, PREGID, TYPE_VISIT, DIAG_VSDAT) %>% 
  mutate(DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "DIAG_VSDAT") %>% 
  mutate(FORM = "MNH06")

mnh07_sub <- mnh07 %>% select(MOMID, PREGID, TYPE_VISIT, MAT_SPEC_COLLECT_DAT) %>%
  mutate(MAT_SPEC_COLLECT_DAT = ymd(parse_date_time(MAT_SPEC_COLLECT_DAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "MAT_SPEC_COLLECT_DAT") %>% 
  mutate(FORM = "MNH07")

mnh08_sub <- mnh08 %>% select(MOMID, PREGID, TYPE_VISIT, LBSTDAT) %>% 
  mutate(LBSTDAT = ymd(parse_date_time(LBSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "LBSTDAT") %>% 
  mutate(FORM = "MNH08")

##MNH09, need to extract the minimum delivery datetime in the event of a multiples pregnancy
mnh09_sub <- mnh09 %>% select(MOMID, PREGID, 
                              DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, 
                              DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,
                              DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
                              DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4)  %>% 
  ## convert delivery date to date class
  mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
  ) %>% 
  # replace default value date with NA 
  mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1==dmy("07-07-1907"), NA), 
         DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==dmy("07-07-1907"), NA),
         DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==dmy("07-07-1907"), NA),
         DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==dmy("07-07-1907"), NA)) %>% 
  # replace default value time with NA 
  mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),  ## some sites reporting 07:07 
         DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
         DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA),
         DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77", NA)) %>% 
  # concatenate date and time of birth 
  mutate(DELIVERY_DATETIME_INF1 = paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1),
         DELIVERY_DATETIME_INF2 = paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2),
         DELIVERY_DATETIME_INF3 = paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3),
         DELIVERY_DATETIME_INF4 = paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4)) %>% 
  # assign time field type for time of birth
  mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M")) %>%
  mutate(DOB = 
           pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, 
                DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
  select(MOMID, PREGID, DOB) %>% 
  mutate(DOB = ymd(DOB))

mnh12_sub <- mnh12 %>% select(MOMID, PREGID, TYPE_VISIT, VISIT_OBSSTDAT) %>% 
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "VISIT_OBSSTDAT") %>% 
  mutate(FORM = "MNH12")

#*****************************************************************************
## Rbind all the data frames together (except delivery and enrollment ultrasound)
#*****************************************************************************
maternal_all <- bind_rows(mnh01_sub, mnh04_sub, mnh05_sub,
                          mnh06_sub, mnh07_sub, mnh08_sub, mnh12_sub)

## Merge in enrollment ultrasound data 
maternal_all <- left_join(maternal_all, mnh01_ga_at_enrl, by = c("MOMID", "PREGID"))

## Merge in DOB data 
maternal_all <- left_join(maternal_all, mnh09_sub, by = c("MOMID", "PREGID"))

#*****************************************************************************
## ANC visits
#*****************************************************************************
## Extract all ANC visits 
anc_visits = c(1,2,3,4,5)
maternal_all_anc = maternal_all %>% filter(TYPE_VISIT %in% anc_visits)

## calculate GA at each vist for ANC visits 
maternal_all_anc <- maternal_all_anc %>% 
  mutate(DIFF_DAYS = as.numeric(VISITDATE - US_OHOSTDAT)) %>%  ## calculate the days difference from the visit to the enrollment ultrasound
  mutate(GA_AT_VISIT_DAYS = GA_US_DAYS + DIFF_DAYS) %>%  ## add the number of days difference to GA at ultrasound to get the estimated GA at visit
  ## convert to weeks
  mutate(GA_AT_VISIT_WKS = floor(GA_AT_VISIT_DAYS/7)) %>% 
  ## double check overlapping anc 32 and anc 36 late windows 
  arrange(-desc(TYPE_VISIT)) %>% 
  #mutate(PREV_VISIT_TYPE = dplyr::lag(TYPE_VISIT, n = 1)) %>% 
  mutate(EXPECTED_TYPE_VISIT = ifelse(GA_AT_VISIT_DAYS >= 126 & GA_AT_VISIT_DAYS <= 181 & GA_US_DAYS <= 125, 2, ## only participants who are <= 17wks at enrollment will have this visit 
                                      ifelse(GA_AT_VISIT_DAYS <=139, 1, 
                                             ifelse(GA_AT_VISIT_DAYS >= 182 & GA_AT_VISIT_DAYS <= 216, 3, 
                                                    ifelse(GA_AT_VISIT_DAYS >= 217 & GA_AT_VISIT_DAYS <= 237, 4, 
                                                           # if ANC32 is missed, it is conducted at ANC36 visit
                                                           ifelse(GA_AT_VISIT_DAYS >= 238 & GA_AT_VISIT_DAYS <= 300, 5, 88))))))


## query any discrepancies 
maternal_all_anc_out <- maternal_all_anc %>% 
  mutate(ERROR = ifelse(TYPE_VISIT != EXPECTED_TYPE_VISIT, "Visit Type Error: Ineligible Window", "No Error")) %>% 
  ## IF enrollment GA is >= 20 wks, they are not eligible for the study -- new query
  mutate(ERROR = ifelse(GA_US_WKS >= 20, "Visit Type Error: Ineligible Enrl GA", ERROR)) %>%  
  ## If GA at enrollment US is >= 18 wks, there should not be a visit type 2 
  mutate(ERROR = ifelse(GA_US_WKS >=18 & GA_US_WKS <20 & TYPE_VISIT == 2, "Visit Type Error: GA>=18wks at Enrl; No ANC20 expected", ERROR)) %>% 
  ## flag and visit dates with 1907-07-07
  mutate(ERROR = ifelse(VISITDATE == "1907-07-07", "Visit Type Error: Invalid visit date (1907-07-07)", ERROR)) %>% 
  ## filter out entries with no errors 
  filter(ERROR != "No Error") %>% 
  mutate(PERIOD = "ANC",
         INFANTID = NA)  


maternal_all_anc_out_full <- maternal_all_anc_out %>% 
  select(FORM, MOMID, PREGID,INFANTID, PERIOD, VISITDATE, GA_AT_VISIT_DAYS, GA_AT_VISIT_WKS, TYPE_VISIT, EXPECTED_TYPE_VISIT, ERROR)

names(maternal_all_anc_out_full) = c("FORM", "MOMID", "PREGID","INFANTID", "PERIOD", "VISIT_DATE",
                                     "AGE_AT_VISIT_DAYS", "AGE_AT_VISIT_WKS",
                                     "REPORTED_TYPE_VISIT", "EXPECTED_TYPE_VISIT", "Error")

## dupliates in mnh01 for visit type = 1 
## GA at us >= 18 weeks should not have visit type == 2 
## default value visit date

#*****************************************************************************
## PNC visits 
#*****************************************************************************
## Extract all PNC visits 
pnc_visits = c(7,8,9,10,11, 12)
maternal_all_pnc = maternal_all %>% filter(TYPE_VISIT %in% pnc_visits) %>% 
  select(-GA_US_DAYS, -GA_US_WKS, -US_OHOSTDAT)

## remove any duplicates -- these will be flagged in the duplicate id code
out_dup_ids <- maternal_all_pnc[duplicated(maternal_all_pnc[,c("MOMID", "PREGID", "TYPE_VISIT", "FORM")]),]
out_dup_ids <- out_dup_ids %>% pull(MOMID)
maternal_all_pnc <- maternal_all_pnc %>% filter(!(MOMID %in% out_dup_ids))


## calculate days PNC at each vist for PNC visits - ONLY USING DAYS -- USE THIS ONE
maternal_all_pnc <- maternal_all_pnc %>% 
  ## need to confirm if this needs to have a +1 to account for day 1 of life 
  mutate(DAYS_PNC = as.numeric(VISITDATE - DOB)) %>%  ## calculate the days difference from the visit to the enrollment ultrasound
  ## convert to weeks
  mutate(WKS_PNC = floor(DAYS_PNC/7)) %>% 
  mutate(EXPECTED_TYPE_VISIT = ifelse(DAYS_PNC >= 3 & DAYS_PNC <= 5, 7, 
                                      ifelse(DAYS_PNC >= 7 & DAYS_PNC <= 14, 8, 
                                             ifelse(DAYS_PNC >= 28 & DAYS_PNC <= 35, 9, 
                                                    ifelse(DAYS_PNC >= 42 & DAYS_PNC <= 104, 10, 
                                                           ifelse(DAYS_PNC >= 182 & DAYS_PNC <= 279, 11,
                                                                  ifelse(DAYS_PNC >= 364 & DAYS_PNC <= 454, 12,
                                                                         ## code unscheduled visits
                                                                         ifelse((DAYS_PNC>0 & DAYS_PNC <= 2) | (DAYS_PNC >=6 & DAYS_PNC < 7) | 
                                                                                  (DAYS_PNC >=15 & DAYS_PNC <=27) | (DAYS_PNC >=36 & DAYS_PNC <= 41) |
                                                                                  (DAYS_PNC >=105 & DAYS_PNC <= 181) | (DAYS_PNC >=280 & DAYS_PNC <= 363), 14, 88))))))))


## query any discrepancies 
maternal_all_pnc_out<- maternal_all_pnc %>% 
  mutate(ERROR = ifelse(TYPE_VISIT != EXPECTED_TYPE_VISIT, "Visit Type Error: Ineligible Window", "No Error")) %>% 
  ##flag any discrepancies between visit type and no DOB reported 
  mutate(ERROR = ifelse(is.na(DOB), "Visit Type Error: PNC visit type reported but missing MNH09 data" , ERROR )) %>% 
  ## flag any default value visit dates "1907-07-07"
  mutate(ERROR = ifelse(VISITDATE == "1907-07-07", "Visit Type Error: Invalid visit date (1907-07-07)", ERROR)) %>% 
  ## flag any pnc visits that occur before the date of birth
  mutate(ERROR = ifelse(VISITDATE < DOB, "Visit Type Error: PNC visit date before reported DOB", ERROR)) %>% 
  filter(ERROR != "No Error") %>%
  mutate(PERIOD = "PNC",
         INFANTID = NA)  


maternal_all_pnc_out_full <- maternal_all_pnc_out %>% 
  select(FORM, MOMID, PREGID,INFANTID, PERIOD, VISITDATE, DAYS_PNC, WKS_PNC, TYPE_VISIT, EXPECTED_TYPE_VISIT, ERROR)

names(maternal_all_pnc_out_full) = c("FORM", "MOMID", "PREGID","INFANTID", "PERIOD", "VISIT_DATE",
                                     "AGE_AT_VISIT_DAYS", "AGE_AT_VISIT_WKS",
                                     "REPORTED_TYPE_VISIT", "EXPECTED_TYPE_VISIT", "Error")


#*****************************************************************************
## generate new tab in query report to store more details  -- mege with infant before exporting
#*****************************************************************************
maternal_visit_types_full <- bind_rows(maternal_all_anc_out_full, maternal_all_pnc_out_full)


#*****************************************************************************
## Format to match query template
#*****************************************************************************
# extract variables included in query template
maternal_all_anc_query <- maternal_all_anc_out %>% 
  mutate(SCRNID = NA,
         INFANTID = NA, 
         VARIABLENAME = "TYPE_VISIT", 
         VARIABLEVALUE = TYPE_VISIT, 
         FIELD_TYPE = "Number", 
         EDIT_TYPE = ifelse(ERROR == "Visit Type Error", paste0("Visit Type Error; TYPE_VISIT = ", TYPE_VISIT, " reported where TYPE_VISIT = ", EXPECTED_TYPE_VISIT, " expected" ), 
                            ERROR)) %>% 
  select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT,VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 



# update naming 
names(maternal_all_anc_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")

# extract variables included in query template
maternal_all_pnc_query <- maternal_all_pnc_out %>% 
  mutate(SCRNID = NA,
         INFANTID = NA, 
         TYPE_VISIT = TYPE_VISIT,
         VARIABLENAME = "TYPE_VISIT", 
         VARIABLEVALUE = TYPE_VISIT, 
         FIELD_TYPE = "Number", 
         EDIT_TYPE = ifelse(ERROR == "Visit Type Error", paste0("Visit Type Error; TYPE_VISIT = ", TYPE_VISIT, " reported where TYPE_VISIT = ", EXPECTED_TYPE_VISIT, " expected" ), 
                            ERROR)) %>% 
  select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 

# update naming 
names(maternal_all_pnc_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")


## merge anc and pnc visit queries together 
MaternalVisitType_query <- bind_rows(maternal_all_anc_query, maternal_all_pnc_query)

if (dim(MaternalVisitType_query)[1] > 1){
  
  ## add additional columns 
  MaternalVisitType_query = cbind(QueryID = NA, 
                                  UploadDate = UploadDate, 
                                  MaternalVisitType_query, 
                                  DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  MaternalVisitType_query$Form_Edit_Type <- paste(MaternalVisitType_query$Form,"_",MaternalVisitType_query$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  MaternalVisitType_query <- MaternalVisitType_query %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "06"))
  
  ## export variable checking query 
  save(MaternalVisitType_query, file = paste0(path_to_save, "MaternalVisitType_query.rda"))
  
}

#*****************************************************************************
#* INFANT VISIT TYPES
#*Export key variables from each dataframe: MOMID, PREGID, VISIT DATE 
#* for MNH01 and 09: export GA and delivery information 
#*****************************************************************************
# MNH09 - extract birth datetime for each infant
mnh09_sub <- mnh09 %>% 
  mutate(DELIV_DSSTDAT_INF1 =  ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF2 =  ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF3 =  ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF4 =  ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
         
  ) %>% 

  mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
         DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>%
  
  # replace default value time with NA 
  mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),
         DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
         DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA),
         DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77", NA)) %>% 
  # concatenate date and time of birth 
  mutate(DELIVERY_DATETIME_INF1 = paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1),
         DELIVERY_DATETIME_INF2 = paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2),
         DELIVERY_DATETIME_INF3 = paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3),
         DELIVERY_DATETIME_INF4 = paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4)) %>% 
  # assign time field type for time of birth
  mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
         DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M"))


## first need to make m09 long format for each infant 
m09_INF1 <- mnh09_sub %>% 
  rename("INFANTID" = "INFANTID_INF1",
         "CES_FAORRES_INF1" = "CES_PROCCUR_INF1") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(as.character(INFANTID)) %>% 
  select(MOMID, PREGID,INFANTID, contains("_INF1")) %>% 
  rename_with(~str_remove(., '_INF1')) 

m09_INF2 <- mnh09_sub %>% rename("INFANTID" = "INFANTID_INF2") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(as.character(INFANTID)) %>% 
  
  select(MOMID, PREGID,INFANTID, contains("_INF2")) %>% 
  rename_with(~str_remove(., '_INF2')) %>% 
  mutate(as.character(DELIV_DSSTDAT)) 

m09_INF3 <- mnh09_sub %>% rename("INFANTID" = "INFANTID_INF3") %>% 
  filter(INFANTID != "n/a") %>% 
  mutate(as.character(INFANTID)) %>% 
  select(MOMID, PREGID,INFANTID, contains("_INF3")) %>% 
  rename_with(~str_remove(., '_INF3')) 

m09_INF4 <- mnh09_sub %>% rename("INFANTID" = "INFANTID_INF4") %>%
  filter(INFANTID != "n/a") %>% 
  mutate(as.character(INFANTID)) %>% 
  select(MOMID, PREGID,INFANTID, contains("_INF4")) %>% 
  rename_with(~str_remove(., '_INF4'))

## bind all infants together 
## if sites have infant id columns that are "blank", remove them from the binding code below. 
# for example, if Ghana site has an empty column for INFANTID_INF4, remove "m09_INF4" below
mnh09_by_inf <- bind_rows(m09_INF1, m09_INF2,  m09_INF3, m09_INF4) 

mnh09_by_inf <- mnh09_by_inf %>% 
  #  mutate(DELIV_DSSTDAT =  ymd(parse_date_time(DELIV_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(DOB = ymd(DELIV_DSSTDAT)) %>% 
  select(MOMID, PREGID, INFANTID, DOB) 


mnh13_sub <- mnh13 %>% select(MOMID, PREGID,INFANTID, TYPE_VISIT, VISIT_OBSSTDAT) %>% 
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "VISIT_OBSSTDAT") %>% 
  mutate(FORM = "MNH13")

mnh14_sub <- mnh14 %>% select(MOMID, PREGID,INFANTID, TYPE_VISIT, VISIT_OBSSTDAT) %>% 
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "VISIT_OBSSTDAT") %>% 
  mutate(FORM = "MNH14")

mnh15_sub <- mnh15 %>% select(MOMID, PREGID,INFANTID, TYPE_VISIT, OBSSTDAT) %>% 
  mutate(OBSSTDAT = ymd(parse_date_time(OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  rename("VISITDATE" = "OBSSTDAT") %>% 
  mutate(FORM = "MNH15")


#*****************************************************************************
## Rbind all the data frames together (except delivery)
#*****************************************************************************
infant_all <- bind_rows(mnh13_sub, mnh14_sub, mnh15_sub)

## Merge in DOB data 
infant_all <- left_join(infant_all, mnh09_by_inf, by = c("MOMID", "PREGID", "INFANTID"))

#*****************************************************************************
## PNC visits 
#*****************************************************************************
## Extract all PNC visits 
pnc_visits = c(7,8,9,10,11, 12)
infant_all_pnc = infant_all %>% filter(TYPE_VISIT %in% pnc_visits)

## remove any duplicates -- these will be flagged in the duplicate id code
out_dup_ids <- infant_all_pnc[duplicated(infant_all_pnc[,c(1,2,3,4,6)]),]
out_dup_ids <- out_dup_ids %>% pull(INFANTID)
infant_all_pnc <- infant_all_pnc %>% filter(!(INFANTID %in% out_dup_ids))


## calculate days PNC at each vist for PNC visits - ONLY USING DAYS -- USE THIS ONE
infant_all_pnc <- infant_all_pnc %>% 
  ## need to confirm if this needs to have a +1 to account for day 1 of life 
  mutate(DAYS_PNC = as.numeric(VISITDATE - DOB)) %>%  ## calculate the days difference from the visit to the enrollment ultrasound
  ## convert to weeks
  mutate(WKS_PNC = floor(DAYS_PNC/7)) %>% 
  mutate(EXPECTED_TYPE_VISIT = ifelse(DAYS_PNC >= 3 & DAYS_PNC <= 5, 7, 
                                      ifelse(DAYS_PNC >= 7 & DAYS_PNC <= 14, 8, 
                                             ifelse(DAYS_PNC >= 28 & DAYS_PNC <= 35, 9, 
                                                    ifelse(DAYS_PNC >= 42 & DAYS_PNC <= 104, 10, 
                                                           ifelse(DAYS_PNC >= 182 & DAYS_PNC <= 279, 11,
                                                                  ifelse(DAYS_PNC >= 364 & DAYS_PNC <= 454, 12,
                                                                         ## code unscheduled visits
                                                                         ifelse((DAYS_PNC>0 & DAYS_PNC <= 2) | (DAYS_PNC >=6 & DAYS_PNC < 7) | 
                                                                                  (DAYS_PNC >=15 & DAYS_PNC <=27) | (DAYS_PNC >=36 & DAYS_PNC <= 41) |
                                                                                  (DAYS_PNC >=105 & DAYS_PNC <= 181) | (DAYS_PNC >=280 & DAYS_PNC <= 363), 14, 88))))))))

## query any discrepancies 
infant_all_pnc_out <- infant_all_pnc %>% 
  mutate(ERROR = ifelse(TYPE_VISIT != EXPECTED_TYPE_VISIT, "Visit Type Error: Ineligible Window", "No Error")) %>% 
  ##flag any discrepancies between visit type and no DOB reported 
  mutate(ERROR = ifelse(is.na(DOB), "Visit Type Error: PNC visit type reported but missing MNH09 data" , ERROR )) %>% 
  ##flag any DOB reported AFTER the visit date
  mutate(ERROR = ifelse(VISITDATE < DOB, "Invalid visit date: PNC visit date before reported DOB" , ERROR )) %>% 
  ## flag and visit dates with 1907-07-07
  mutate(ERROR = ifelse(VISITDATE == "1907-07-07", "Visit Type Error: Invalid visit date (1907-07-07)", ERROR)) %>% 
  filter(ERROR != "No Error") %>% 
  mutate(PERIOD = "PNC")


infant_all_pnc_out_full <- infant_all_pnc_out %>% 
  select(FORM, MOMID, PREGID, INFANTID, PERIOD, VISITDATE, DAYS_PNC, WKS_PNC, TYPE_VISIT, EXPECTED_TYPE_VISIT, ERROR)

names(infant_all_pnc_out_full) = c("FORM", "MOMID", "PREGID","INFANTID", "PERIOD", "VISIT_DATE",
                                   "AGE_AT_VISIT_DAYS", "AGE_AT_VISIT_WKS",
                                   "REPORTED_TYPE_VISIT", "EXPECTED_TYPE_VISIT", "Error")

#*****************************************************************************
## Format extra tab with more detail
#*****************************************************************************
visit_type_extra_tab <- bind_rows(maternal_visit_types_full, infant_all_pnc_out_full)

if (dim(visit_type_extra_tab)[1] > 1){
  
  ## export 
  save(visit_type_extra_tab, file = paste0(path_to_save, "visit_type_extra_tab.rda"))
  
}
#*****************************************************************************
## Format to match query template
#*****************************************************************************
# extract variables included in query template
infant_all_pnc_query <- infant_all_pnc_out %>% 
  mutate(SCRNID = NA,
         TYPE_VISIT = TYPE_VISIT,
         VARIABLENAME = "TYPE_VISIT", 
         VARIABLEVALUE = TYPE_VISIT, 
         FIELD_TYPE = "Number", 
         EDIT_TYPE = ifelse(ERROR == "Visit Type Error", paste0("Visit Type Error; TYPE_VISIT = ", TYPE_VISIT, " reported where TYPE_VISIT = ", EXPECTED_TYPE_VISIT, " expected" ), 
                            ERROR)) %>% 
  select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 

# update naming 
names(infant_all_pnc_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")

## rename dataframe
InfantVisitType_query <- infant_all_pnc_query

if (dim(InfantVisitType_query)[1] > 1){
  
  ## add additional columns 
  InfantVisitType_query = cbind(QueryID = NA, 
                                UploadDate = UploadDate, 
                                InfantVisitType_query, 
                                DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  InfantVisitType_query$Form_Edit_Type <- paste(InfantVisitType_query$Form,"_",InfantVisitType_query$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  InfantVisitType_query <- InfantVisitType_query %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",InfantID, "_",`Variable Name`, "_", `Variable Value`, "_", "06"))
  
  ## export 
  save(InfantVisitType_query, file = paste0(path_to_save, "InfantVisitType_query.rda"))
  
}
