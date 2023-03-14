#*****************************************************************************
#*QUERY #4 -- CHECK THAT VISIT TYPE AND GA AND/OR PNC DAYS MATCH WITH WHAT IS REPORTED 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 13 March 2023

#*Input: Wide data (all raw .csv files)
#*Function: check for visit types that do not match 
#*Output: .rda file with all mismatched visit types 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Update "UploadDate" 
#* 2. Set working directory to site-specific folder  
#* 3. Load in wide data 

#* Once the previous lines of code are updated, you can start to run the script 

#* Notes: 
#* two .rda files will be saved: 
#* 1. the .rda file that will be included in the query report 
#* 2. a .rda file that has the same output as the query report but with some added notes -- good to review 

#*****************************************************************************

# clear environment 
rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" 
UploadDate = "2023-02-10"

#*****************************************************************************
#* load data
#*****************************************************************************
## UPDATE EACH RUN ## 
## 2. Set working directory to site-specific folder -- main folder 
setwd("~/PRiSMAv2Data/Kenya/2023-02-10/")

## UPDATE EACH RUN ## 
## 3. Load in wide data 
load("~/PRiSMAv2Data/Kenya/2023-02-10/data/2023-02-10_wide.RData")
#*****************************************************************************
#*List of important variables: 
    #*ANC_OBSSTDAT       # mnh04 visit date 
    #*ANT_PEDAT          # mnh05 visit date 
    #*DIAG_VSDAT         # mnh06 visit date 
    #*VISIT_OBSSTDAT     # mnh13 visit date 
    #*VISIT_OBSSTDAT     # mnh14 visit date 
    #*OBSSTDAT           # mnh15 date 
    #*DELiV_DSSTDAT_INF1 # date of delivery 
#*****************************************************************************

## only extract key variables from each of the forms that check for visit type  
mnh01_merge <- mnh01 %>% select(SCRNID,CAL_GA_WKS_AGE_FTS1, US_OHOSTDAT)
mnh02_merge <- mnh02 %>% select(SCRNID, MOMID, PREGID, SCRN_OBSSTDAT)
mnh04_merge <- mnh04 %>% select(MOMID, PREGID, TYPE_VISIT, ANC_OBSSTDAT) 
mnh05_merge <- mnh05 %>% select(MOMID, PREGID, TYPE_VISIT, ANT_PEDAT)
mnh06_merge <- mnh06 %>% select(MOMID, PREGID, TYPE_VISIT, DIAG_VSDAT)
mnh09_merge <- mnh09 %>% select(MOMID, PREGID, DELIV_DSSTDAT_INF1)  %>% rename("DeliveryDate" = "DELIV_DSSTDAT_INF1") 
mnh09_merge$DeliveryDate = mdy_hm(mnh09_merge$DeliveryDate)
mnh12_merge <- mnh12 %>% select(MOMID, PREGID, PNC_N_VISIT, VISIT_OBSSTDAT) 

## since mnh12 has a different visit type variable, we need to reassign the values before we merge 
mnh12_merge$TYPE_VISIT <- ifelse(mnh12_merge$VISIT_OBSSTDAT==1, mnh12_merge$TYPE_VISIT == 8, 
                                 ifelse(mnh12_merge$VISIT_OBSSTDAT==2, mnh12_merge$TYPE_VISIT == 9, 
                                        ifelse(mnh12_merge$VISIT_OBSSTDAT==3, mnh12_merge$TYPE_VISIT == 10, 
                                               ifelse(mnh12_merge$VISIT_OBSSTDAT==4, mnh12_merge$TYPE_VISIT == 11, 
                                                      ifelse(mnh12_merge$VISIT_OBSSTDAT==5, mnh12_merge$TYPE_VISIT == 12, 
                                                             ifelse(mnh12_merge$VISIT_OBSSTDAT==6, mnh12_merge$TYPE_VISIT == 13, NA))))))

mnh12_merge = mnh12_merge %>% select(MOMID, PREGID, TYPE_VISIT, VISIT_OBSSTDAT)

## continue to merge all of the data 
out <- merge(mnh01_merge, mnh02_merge, by = "SCRNID") 
out_m04 <- merge(out, mnh04_merge, by = c("MOMID", "PREGID")) %>% rename("VisitDate" = "ANC_OBSSTDAT") %>% mutate(form = "MNH04")
out_m05 <- merge(out, mnh05_merge, by = c("MOMID", "PREGID")) %>% rename("VisitDate" = "ANT_PEDAT") %>% mutate(form = "MNH05")
out_m06 <- merge(out, mnh06_merge, by = c("MOMID", "PREGID")) %>% rename("VisitDate" = "DIAG_VSDAT") %>% mutate(form = "MNH06")
out_m12 <- merge(out, mnh12_merge, by = c("MOMID", "PREGID")) %>% rename("VisitDate" = "VISIT_OBSSTDAT")  %>% mutate(form = "MNH12")

out_vt <- rbind(out_m04, out_m05, out_m06, out_m12)
out_vt <- left_join(out_vt, mnh09_merge,  by = c("MOMID", "PREGID")) ## merge in delivery information

## convert all date variables to date types  
out_vt$US_OHOSTDAT = mdy_hm(out_vt$US_OHOSTDAT)
out_vt$VisitDate = mdy_hm(out_vt$VisitDate)

## for forms that are filled out at every visit, we can differentiate by defining if a visit is during PNC or not 
## pnc period is defined as the visit date being AFTER the delivery date 
out_vt$PNC_YN <- ifelse(is.na(out_vt$DeliveryDate) | out_vt$VisitDate<out_vt$DeliveryDate, 0, 1)

## calculate gestational age at each visit 
out_vt$GA_DAYS_US <- ifelse(out_vt$PNC_YN == 0, floor(out_vt$CAL_GA_WKS_AGE_FTS1*7 + out_vt$CAL_GA_DAYS_AGE_FTS1), 0)
out_vt$GA_AT_VISIT_DAYS <- ifelse(out_vt$PNC_YN == 0, floor(as.numeric(difftime(out_vt$VisitDate,out_vt$US_OHOSTDAT, units = "days")) + out_vt$GA_DAYS_US),0)
out_vt$GA_AT_VISIT_WKS <- floor(out_vt$GA_AT_VISIT_DAYS/7)

## calculate days since birth at each visit for PNC visits 
out_vt$DAYS_PNC <- ifelse(out_vt$PNC_YN == 1,  floor(as.numeric(difftime(out_vt$VisitDate,out_vt$DeliveryDate, units = "days"))), 0)
out_vt$WKS_PNC <- floor(out_vt$DAYS_PNC/7)

## assign the visit type based on window 
out_vt$EXPECTED_VISIT = ifelse((out_vt$PNC_YN == 0 & out_vt$GA_AT_VISIT_WKS <=17)| (out_vt$GA_AT_VISIT_WKS >= 18 & out_vt$GA_AT_VISIT_WKS <= 25 & out_vt$PNC_YN == 0 & out_vt$VisitDate==out_vt$US_OHOSTDAT & out_vt$PNC_YN == 0), 1,
                               ifelse(((out_vt$PNC_YN == 0 &out_vt$GA_AT_VISIT_WKS >= 18 & out_vt$GA_AT_VISIT_WKS <= 25) & out_vt$VisitDate>out_vt$US_OHOSTDAT), 2, 
                                      ifelse(out_vt$PNC_YN == 0 &out_vt$GA_AT_VISIT_WKS>= 26 & out_vt$GA_AT_VISIT_WKS <= 30, 3, 
                                             ifelse(out_vt$PNC_YN == 0 &out_vt$GA_AT_VISIT_WKS >= 31 & out_vt$GA_AT_VISIT_WKS <= 33, 4, 
                                                    ifelse(out_vt$PNC_YN == 0 & out_vt$GA_AT_VISIT_WKS >= 33, 5, ## need to add pregancy visit somehow 
                                                           ifelse(out_vt$PNC_YN == 1 & (out_vt$DAYS_PNC >= 3 & out_vt$DAYS_PNC <= 5), 7,
                                                                  ifelse(out_vt$PNC_YN == 1 & (out_vt$DAYS_PNC >= 7 & out_vt$DAYS_PNC <= 14), 8, 
                                                                         ifelse(out_vt$PNC_YN == 1 & (out_vt$DAYS_PNC >= 28 & out_vt$DAYS_PNC <= 35), 9,
                                                                                ifelse(out_vt$PNC_YN == 1 & (out_vt$WKS_PNC >= 8 & out_vt$WKS_PNC <= 12), 10,
                                                                                       ifelse(out_vt$PNC_YN == 1 & (out_vt$WKS_PNC >= 29 & out_vt$WKS_PNC <= 39), 11,
                                                                                              ifelse(out_vt$PNC_YN == 1 & (out_vt$WKS_PNC >= 55 & out_vt$WKS_PNC <= 64), 12,NA)))))))))))



## remove if the day between enrollment and anc20 is less than a day 
## identify any discrepancies 
out_vt$outrange <- ifelse(out_vt$EXPECTED_VISIT == out_vt$TYPE_VISIT | out_vt$VisitDate - out_vt$US_OHOSTDAT <=1, "", out_vt$TYPE_VISIT)

## double check that those who had a visit #2 had a first visit -- if so, they should be removed - this is due to overlapping windows with enrollment and ANC20 visit 
out_vt <- 
  out_vt %>%
  arrange(VisitDate) %>% 
  group_by(MOMID, form) %>%
  mutate(PREV_VISIT_TYPE = dplyr::lag(TYPE_VISIT, n = 1))


## remove queries pulled cause by anc20 vs enrollment period 
# make edit message 
out_vt$editmessage <- ifelse(out_vt$outrange == "" | 
                            (out_vt$outrange == 2 & out_vt$EXPECTED_VISIT == 1 & out_vt$PREV_VISIT_TYPE==1) | 
                            (out_vt$outrange == 1 & out_vt$EXPECTED_VISIT == 2 & is.na(out_vt$PREV_VISIT_TYPE)), "NoError", "Visit Type Error") ## remove the women who have overlappign windows for enrollment and ANC and were flagged 

# filter out those that are out of range 
out_vt_query <- out_vt %>% filter(editmessage == "Visit Type Error")

# export out 
## only keep the first 7 columns 
out_vt_query = out_vt_query %>% select(MOMID, PREGID, SCRNID, TYPE_VISIT,EXPECTED_VISIT, form, GA_AT_VISIT_WKS, DAYS_PNC, VisitDate)
VisitTypeRangeQuery_withNotes <- out_vt_query

# reorder
##rearrange columns 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}

# rearrange columns 
VisitTypeRangeQuery_withNotes <- VisitTypeRangeQuery_withNotes %>% bind_cols(INFANTID=NA, VisitType = "n/a", varname = "TYPE_VISIT")  %>% 
  setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitType, VisitDate, form, varname, TYPE_VISIT,EXPECTED_VISIT, GA_AT_VISIT_WKS, DAYS_PNC)

# replace 0s with NAs 
VisitTypeRangeQuery_withNotes$GA_AT_VISIT_WKS <- ifelse(VisitTypeRangeQuery_withNotes$GA_AT_VISIT_WKS==0, NA, VisitTypeRangeQuery_withNotes$GA_AT_VISIT_WKS)
VisitTypeRangeQuery_withNotes$DAYS_PNC <- ifelse(VisitTypeRangeQuery_withNotes$DAYS_PNC==0, NA, VisitTypeRangeQuery_withNotes$DAYS_PNC)

# only extract variables for report 
VisitTypeRangeQuery <- VisitTypeRangeQuery_withNotes %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitType, VisitDate, form, varname, TYPE_VISIT)

# update naming
names(VisitTypeRangeQuery) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "DateFormCompleted", "Form", "Variable Name", "Variable Value")


## add additional columns 
if (nrow(VisitTypeRangeQuery)>=1){
  VisitTypeRangeQuery = cbind(QueryID = NA, 
                       UploadDate = UploadDate, 
                       #MomID = "NA", PregID = "NA",
                       #DateFormCompleted = "NA", 
                       VisitTypeRangeQuery, 
                       #`Variable Name` = "NA",
                       FieldType = "Number", 
                       EditType = "Visit Type Error", 
                       DateEditReported = format(Sys.time(), "%Y-%m-%d")) 
}

# add form edit type column 
VisitTypeRangeQuery$Form_Edit_Type <- paste(VisitTypeRangeQuery$Form,"_",VisitTypeRangeQuery$EditType)

# set Date form completed to character class 
VisitTypeRangeQuery$DateFormCompleted = as.character(VisitTypeRangeQuery$DateFormCompleted)

VisitTypeRange_query  <- VisitTypeRangeQuery
save(VisitTypeRange_query, file = "queries/VisitTypeRange_query.rda")                # this version matches the query template 
save(VisitTypeRangeQuery_withNotes, file = "queries/VisitTypeRange_query_Notes.rda") # this version has notes that include GA at visit, days PNC, and expected visit 

  

