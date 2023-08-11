#*****************************************************************************
#*QUERY #5 -- CONFIRM ALL "ENROLLED" PARTICIPANTS MEET OUR ENROLLMENT ELIGIBILITY CRITERIA
#*Input: Wide data (all raw .csv files) & Long data
#*Function: Confirm all enrolled particpants meet our enrollment criteria as in MNH02
#*Output: .rda file with all MOMIDs that do not meet enrollment criteria 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* Last updated: 24 July 2023
  ## updates to include type visit in the final output
#* Previous updates: 30 May 2023
# Moved query ID code from export script to this script

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for

#* Once the previous lines of code are updated, you can start to run the script 
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
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
UploadDate = "2023-07-21"
site = "Ghana"

#*****************************************************************************
#* load data
#*****************************************************************************
## Set working directory to site-specific folder -- main folder
setwd(paste0("~/PRiSMAv2Data/", site, "/", UploadDate, "/", sep = ""))

## Load in long data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_long.Rdata", sep = "")) 

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 
#*****************************************************************************

## extract MOMIDs in all forms following MNH02. We will assume that any participant with these forms are considered "enrolled" by sites 
all_momid <- data_long %>% filter(form != "MNH02" & form != "MNH00" & form != "MNH01") %>% distinct(MOMID) %>% pull(MOMID)

## subset all MOMIDs that have forms 03-25 from MNH02 (enrollment form)
momid_enroll_forms <- mnh02 %>% filter(MOMID %in% all_momid)

## Among these women who the sites are considering "enrolled", do they meet our enrollment criteria?
# 1, Yes meets enrollment criteria 
# 0, No does not meet enrollment criteria
momid_enroll_forms$MEET_ENROLL_CRIT = ifelse(momid_enroll_forms$AGE_IEORRES == 1 & momid_enroll_forms$PC_IEORRES == 1 & 
                                               momid_enroll_forms$CATCHMENT_IEORRES == 1 & momid_enroll_forms$CATCH_REMAIN_IEORRES == 1 &
                                               momid_enroll_forms$CONSENT_IEORRES == 1, 1, 0)

## Extract only MOMIDs that do not meet enrollment criteria (MEET_ENROLL_CRIT == 0)
momid_enroll_forms <- momid_enroll_forms %>% filter(MEET_ENROLL_CRIT==0)

## Update formatting - we only want to keep certain columns
momid_enroll_forms = momid_enroll_forms %>% select(SCRNID, MOMID, PREGID, SCRN_OBSSTDAT) %>% 
  mutate(InfantID = NA, 
         Form = "MNH02",
         TYPE_VISIT = NA)

## reorder columns 
momid_enroll_forms <- momid_enroll_forms[, c("SCRNID", "MOMID", "PREGID", "InfantID","VisitType", "SCRN_OBSSTDAT", "Form")] 

## update naming
names(momid_enroll_forms) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form")

# rename data frame 
MomidNotEligible <- momid_enroll_forms

if (dim(MomidNotEligible)[1] >= 1){
  
  ## add additional columns 
  MomidNotEligible_query = cbind(QueryID = NA, 
                                 UploadDate = UploadDate, 
                                 #MomID = "NA", PregID = "NA",
                                 #VisitDate = "NA", 
                                 MomidNotEligible, 
                                 `Variable Name` = "NA",
                                 `Variable Value` = "NA",
                                 FieldType = "Text", 
                                 EditType = "MomID Ineligible", 
                                 DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  MomidNotEligible_query <- add_column(MomidNotEligible_query,Form_Edit_Type = paste(MomidNotEligible_query$Form,"_",MomidNotEligible_query$EditType))
  
  ## assign queryid -- 
  # edit type id for momid inelgibile is 08
  MomidNotEligible_query <- MomidNotEligible_query %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "08"))
  
  #export Mom ID missing enrollment criteria query 
  save(MomidNotEligible_query, file = "queries/MomidNotEligible_query.rda")
  
}
