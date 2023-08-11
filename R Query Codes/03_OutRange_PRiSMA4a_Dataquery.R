#*****************************************************************************
#*QUERY #3 -- CHECK FOR OUT OF RANGE VALUES 
#* Written by: Stacie Loisate & Xiaoyan Hu

#* Last updated: 15 June 2023
       # Lines 529, 616, 703, 790, 878: Updated mnh25 codes by adding in a line extracting the continuous variables 
       # Lines 537, 623, 710, 798, 886: Updated mnh25 codes by updated the EPDS score ranges (this was updated in data dictionary, but not in the codes; now is resolved)
       # Lines 256-257 & 317-318: Added codes to remove all mnh25 variables from the continous and categorial variable checking codes 

#*Input: Long data 
#*Function: check for any out of range values  
#*Output: .rda file with all out of range values 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for 

#* Once the previous lines of code are updated, you can start to run the script 

#* Notes: 
#* make sure the data dictionary and fetal biometry excel sheets are in the right folder
#* At the end, 2 tables will appear -- these are to check for default values -- good just to eyeball them 

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
UploadDate = "2023-06-09"
site = "Ghana"

# make reorder function to use for later 
##rearrange columns 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}

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
#*import data dictionary 
#*****************************************************************************
#* create excel work book 
#varNames_sheet <- read_excel("~/PRiSMAv2Data/Queries/PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_Queries.xlsx")
varNames_sheet <- read_excel("~/PRiSMAv2Data/Queries/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
varNames_sheet <- varNames_sheet %>% dplyr::select(Form, `Variable Name`, `Response Options`, `Query Category`,`Value`, `Field Type (Date, Time, Number, Text)`, `Minimum Value`, `Maximum Value`)
varNames_sheet$`Variable Name` = toupper(varNames_sheet$`Variable Name`)

## rename variables in data dictionary
varNames_sheet <- varNames_sheet %>% rename("varname" = `Variable Name`, "form" = "Form", "ResponseRange" = `Value`)
#*****************************************************************************
#* Fetal Biometry Checks 
#******************************************************************************
#* CAL_GA_WKS_AGE_FTS1 -- estimated GA in days by ACOG 
#* FL_PERES_01_FTS1; FL_PERES_MEAN_FTS1 
#* AC_PERES_01_FTS1; AC_PERES_MEAN_FTS1
#* HC_PERES_01_FTS1; HC_PERES_MEAN_FTS1
#* BPD_PERES_01_FTS1; BPD_PERES_MEAN_FTS1
fetalRange_sheet <- read_excel("~/PRiSMAv2Data/Queries/fetal_biometry_range.xlsx")

## will need to update US_TYPE or TYPE_VISIT depending on what sites are reporting 
fetal_biometry_vars = c("SCRNID","MOMID","PREGID","US_OHOSTDAT", "TYPE_VISIT", "US_VISIT", 
                        "US_GA_WKS_AGE_FTS1", "US_GA_DAYS_AGE_FTS1", "US_GA_WKS_AGE_FTS2", "US_GA_DAYS_AGE_FTS2", ##  GA 
                        "US_GA_WKS_AGE_FTS3", "US_GA_DAYS_AGE_FTS3", "US_GA_WKS_AGE_FTS4", "US_GA_DAYS_AGE_FTS4", 
                        "FL_PERES_01_FTS1", "FL_PERED_01_FTS2", "FL_PERED_01_FTS3", "FL_PERED_01_FTS4", ## first measurement of FL 
                        "FL_PERES_02_FTS1", "FL_PERED_02_FTS2", "FL_PERED_02_FTS3", "FL_PERED_02_FTS4", ## second measurement of FL
                        "FL_PERES_MEAN_FTS1", "FL_PERES_MEAN_FTS2", "FL_PERES_MEAN_FTS3", "FL_PERES_MEAN_FTS4",   ## FL mean 
                        "AC_PERES_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", ## first measurement of AC 
                        "AC_PERES_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", ## second measurement of AC 
                        "AC_PERES_MEAN_FTS1", "AC_PERES_MEAN_FTS2", "AC_PERES_MEAN_FTS3", "AC_PERES_MEAN_FTS4", ## AC mean
                        "HC_PERES_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", ## first measurement of HC
                        "HC_PERES_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", ## second measurement of HC
                        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4", ## HC mean 
                        "BPD_PERES_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", ## first measurement of bpd 
                        "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", ## second measurement of bpd 
                        "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4") ## BPD mean 

# extract fetal biometry  variables from the data 
#forms_fetal_biomery <- data_long %>% filter(varname %in% fetal_biometry_vars)
forms_fetal_biomery <- mnh01 %>% select(any_of(fetal_biometry_vars))

# convert visit date variable to date class 
forms_fetal_biomery$US_OHOSTDAT = parse_date_time(forms_fetal_biomery$US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%m/%d/%Y %H:%M"))
forms_fetal_biomery$US_OHOSTDAT = ymd(forms_fetal_biomery$US_OHOSTDAT)

# extract ultrasound visit 1 and calculate GA_US_BASLINE_DAYS 
forms_fetal_biomery_visit1 <- forms_fetal_biomery %>% 
  filter(TYPE_VISIT == 1) %>% 
  ## identify max GA 
  mutate(GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA)) %>% 
  mutate(GestAge_Days = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
  select(-GA_US_DAYS_FTS1, -GA_US_DAYS_FTS2, -GA_US_DAYS_FTS3, -GA_US_DAYS_FTS4)

## in order to calculate GA at at the other US visits, we need to merge in the GA at screening date with the subset of data for the other visits 
  ## below i am creating a subset of the  forms_fetal_biomery_visit1 dataset to merge 
visit1_to_merge <- forms_fetal_biomery_visit1 %>% 
    # rename visit date to "baseline date" - this will help with merging 
    rename("BASELINE_DATE" = US_OHOSTDAT) %>% 
    rename("GA_US_BASELINE_DAYS" = GestAge_Days) %>% 
    select(SCRNID, BASELINE_DATE, GA_US_BASELINE_DAYS)
  
# extract non-screening ultrasound visits  
forms_fetal_biomery_other_visits <- forms_fetal_biomery %>% 
  filter(TYPE_VISIT != 1) 

# merge screening visit and non-screening visits (we need GA_US_BASELINE_DAYS as its own column so we can calculate GA at visit)
forms_fetal_biomery_cal_ga <- left_join(forms_fetal_biomery_other_visits, visit1_to_merge, by = c("SCRNID"))
forms_fetal_biomery_cal_ga <- forms_fetal_biomery_cal_ga %>% 
  # calculate the difference in days between US visits 
  mutate(DIFF_DAYS = as.numeric(US_OHOSTDAT-BASELINE_DATE)) %>% 
  # add the days difference to the GA at screening ultrasound to get the expected GA at non-screening ultrasound
  mutate(GestAge_Days = GA_US_BASELINE_DAYS + DIFF_DAYS) %>% 
  select(-BASELINE_DATE, -GA_US_BASELINE_DAYS, -DIFF_DAYS)

## rbind all visits together
forms_fetal_biomery_all_visits <- bind_rows(forms_fetal_biomery_visit1, forms_fetal_biomery_other_visits)
forms_fetal_biomery_all_visits <- forms_fetal_biomery_all_visits %>% 
  relocate("GestAge_Days", .after = "PREGID") %>% 
  select(-contains("US_GA_WKS_AGE_FTS"), -contains("US_GA_DAYS_AGE_FTS")) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(1:6), 
               names_to = "varname", values_to = "response") %>% 
  rename(VisitDate = "US_OHOSTDAT") %>% 
  ## make sure GA is floored 
  mutate(GestAge_Days = floor(as.numeric(GestAge_Days)))

## merge fetal biometry excel sheet and the data 
forms_fetal_biomery_ranges <- merge(forms_fetal_biomery_all_visits, fetalRange_sheet, by = "GestAge_Days")

# query BPD 
bpd <- c("BPD_PERES_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", ## first measurement of bpd 
         "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", ## second measurement of bpd 
         "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4")## BPD mean 

forms_fetal_biomery_bpd <- forms_fetal_biomery_ranges %>% filter(varname %in% bpd, response !=-7) ## remove default value
forms_fetal_biomery_bpd$outrange <- ifelse((forms_fetal_biomery_bpd$response >= forms_fetal_biomery_bpd$bpd_min & forms_fetal_biomery_bpd$response <= forms_fetal_biomery_bpd$bpd_max) | forms_fetal_biomery_bpd$response!= "-7", "", forms_fetal_biomery_bpd$response)

# query HC
hc <- c("HC_PERES_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", ## first measurement of HC
        "HC_PERES_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", ## second measurement of HC
        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4") ## HC mean 

forms_fetal_biomery_hc <- forms_fetal_biomery_ranges %>% filter(varname %in% hc, response !=-7)
forms_fetal_biomery_hc$outrange <- ifelse((forms_fetal_biomery_hc$response >= forms_fetal_biomery_hc$hc_min & forms_fetal_biomery_hc$response <= forms_fetal_biomery_hc$hc_max) | forms_fetal_biomery_hc$response!= "-7", "", forms_fetal_biomery_hc$response)

# query AC 
ac <- c("AC_PERES_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", ## first measurement of AC 
        "AC_PERES_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", ## second measurement of AC 
        "AC_PERES_MEAN_FTS1", "AC_PERES_MEAN_FTS2", "AC_PERES_MEAN_FTS3", "AC_PERES_MEAN_FTS4") ## AC mean

forms_fetal_biomery_ac <- forms_fetal_biomery_ranges %>% filter(varname %in% ac, response !=-7)
forms_fetal_biomery_ac$outrange <- ifelse((forms_fetal_biomery_ac$response >= forms_fetal_biomery_ac$ac_min & forms_fetal_biomery_ac$response <= forms_fetal_biomery_ac$ac_max) | forms_fetal_biomery_ac$response!= "-7", "", forms_fetal_biomery_ac$response)

# query FL 
fl <- c("FL_PERES_01_FTS1", "FL_PERED_01_FTS2", "FL_PERED_01_FTS3", "FL_PERED_01_FTS4", ## first measurement of FL 
        "FL_PERES_02_FTS1", "FL_PERED_02_FTS2", "FL_PERED_02_FTS3", "FL_PERED_02_FTS4", ## second measurement of FL
        "FL_PERES_MEAN_FTS1", "FL_PERES_MEAN_FTS2", "FL_PERES_MEAN_FTS3", "FL_PERES_MEAN_FTS4")  ## FL mean 

forms_fetal_biomery_fl <- forms_fetal_biomery_ranges %>% filter(varname %in% fl, response !=-7)
forms_fetal_biomery_fl$outrange <- ifelse((forms_fetal_biomery_fl$response >= forms_fetal_biomery_fl$fl_min & forms_fetal_biomery_fl$response <= forms_fetal_biomery_fl$fl_max) | forms_fetal_biomery_fl$response!= "-7", "", forms_fetal_biomery_fl$response)

# rbind all fetal biometry measurements 
forms_fetal_biometry_all <- rbind(forms_fetal_biomery_bpd, forms_fetal_biomery_hc, forms_fetal_biomery_ac, forms_fetal_biomery_fl)

# make edit message 
forms_fetal_biometry_all$editmessage <- ifelse(forms_fetal_biometry_all$outrange == "", "NoError", "Out of Range")

# filter out those that are out of range 
forms_fetal_biometry_query <- forms_fetal_biometry_all %>% filter(editmessage == "Out of Range")

## only keep the first 7 columns 
forms_fetal_biometry_query = forms_fetal_biometry_query %>% select(SCRNID, MOMID, PREGID, TYPE_VISIT, VisitDate, varname, response)

## add infant ID  column
forms_fetal_biometry_query <- add_column(forms_fetal_biometry_query, InfantID = NA , .after = "PREGID")

## add form column
forms_fetal_biometry_query <- add_column(forms_fetal_biometry_query, Form = "MNH01" , .after = "VisitDate")

FetalBioRangeQuery <- forms_fetal_biometry_query

# update naming
names(FetalBioRangeQuery) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(FetalBioRangeQuery)>=1){
  FetalBioRangeQuery = cbind(QueryID = NA, 
                             UploadDate = UploadDate, 
                             #MomID = "NA", PregID = "NA",
                             #VisitDate = "NA", 
                             FetalBioRangeQuery, 
                             #`Variable Name` = "NA",
                             FieldType = "Number", 
                             EditType = "Out of Range", 
                             DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(FetalBioRangeQuery)>=1){
  FetalBioRangeQuery$`Variable Value` = as.character(FetalBioRangeQuery$`Variable Value`)
}

#*****************************************************************************
#*Categorical Variable Queries 
#*****************************************************************************

## merge data dictionary and site data  
invalid_response_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
## make vector of MNH25 forms - we will check these separately at the end 
m25_forms = c("MNH25_Ghana", "MNH25_India", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan")
invalid_response_categorical <- invalid_response_merge %>% filter(`Query Category` == "Number", !(form %in% m25_forms))

# make the response range a vector 
invalid_response_categorical$response_range = as.vector(c(invalid_response_categorical$`ResponseRange`))

# run the query
invalid_response_categorical$in_range <- apply(invalid_response_categorical,1, function(x){
  grepl(x["response"], x["response_range"])
})

# make edit message 
invalid_response_categorical$editmessage <- ifelse(invalid_response_categorical$in_range == "FALSE", "Invalid Response Option", "No Error") 

# filter out those that are out of range 
invalid_response_categorical_query <- invalid_response_categorical %>% filter(editmessage == "Invalid Response Option")

# get tab of variables that have default values 
default_values <- c("55", "88", "77", "99", "66")
invalid_response_default_value <- invalid_response_categorical %>% 
  group_by(varname) %>% 
  add_count(name = "n_total") %>%                    ## get the total count for each variable 
  filter(editmessage == "Invalid Response Option") %>%          ## only look at the variables that do not include default values in the response options
  group_by(form, varname, response,n_total) %>%      ## group by variable name, response, total, and form
  count(name ="n_response") %>%                      ## count the number each specific response is reported 
  mutate(pcnt_total = (n_response/n_total)*100) %>%  ## get percentange each reponse 
  filter(response %in% default_values)               ## exclude to only the default values 

# remove default values for query but will review at the end of script 
invalid_response_categorical_query <- invalid_response_categorical_query %>% filter(!(response %in% default_values)) 

# remove MNH25 variables -- we will check these later 
invalid_response_categorical_query <- invalid_response_categorical_query %>% filter(form != "MNH25")

## only keep the first 7 columns 
InvalidResponseCategoricalQuery = invalid_response_categorical_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)

# update naming
names(InvalidResponseCategoricalQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(InvalidResponseCategoricalQuery)>=1){
  InvalidResponseCategoricalQuery = cbind(QueryID = NA, 
                                          UploadDate = UploadDate, 
                                          #MomID = "NA", PregID = "NA",
                                          #VisitDate = "NA", 
                                          InvalidResponseCategoricalQuery, 
                                          #`Variable Name` = "NA",
                                          FieldType = "Number", 
                                          EditType = "Invalid Response Option", 
                                          DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(InvalidResponseCategoricalQuery)>=1){
  InvalidResponseCategoricalQuery$`Variable Value` = as.character(InvalidResponseCategoricalQuery$`Variable Value`)
}
#*****************************************************************************
#*Continuous 
#*****************************************************************************
# extract continuous variables from the data dictionary 
## make vector of MNH25 forms - we will check these separately at the end 
m25_forms = c("MNH25_Ghana", "MNH25_India", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan")
requested_varNames_out <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (!is.na(`Minimum Value`) | !is.na(`Maximum Value`)), 
                                                    !(`varname` %in% fetal_biometry_vars), !(form %in% m25_forms)) 
form_num = toupper(form_num)
requested_varNames_out_var <- requested_varNames_out %>% filter(form %in% form_num) %>% 
  #rename("varname" = `Variable Name`) %>% 
  pull(varname)

# extract the continuous variables that don't have min max values -- save for later 
requested_varNames_Nominmax <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (is.na(`Minimum Value`) | is.na(`Maximum Value`)))

# extract continuous variables from the data 
forms_con <- data_long %>% filter(varname %in% requested_varNames_out_var)

# merge together the min/max value columns 
requested_varNames_out_w_MinMax <- requested_varNames_out %>% dplyr::select(form, `varname`, `Minimum Value`, `Maximum Value`) #`DefaultValue`)
names(requested_varNames_out_w_MinMax) = c("form", "varname", "min", "max") #, "DefaultValue")
forms_con_merged <- merge(forms_con, requested_varNames_out_w_MinMax, by = c("form", "varname"))

# make sure response options, min, and max values are numeric 
forms_con_merged$response = as.numeric(forms_con_merged$response)
forms_con_merged$min = as.numeric(forms_con_merged$min)
forms_con_merged$max = as.numeric(forms_con_merged$max)
#forms_con_merged$DefaultValue = as.numeric(forms_con_merged$DefaultValue)

# query -- if greater than min and lower than max column or does not equaly the default value 
forms_con_merged$outrange <- ifelse((forms_con_merged$response >= forms_con_merged$min & forms_con_merged$response <= forms_con_merged$max), "", forms_con_merged$response)

# make edit message 
forms_con_merged$editmessage <- ifelse(forms_con_merged$outrange == "", "NoError", "Out of Range")

# filter out those that are out of range 
forms_con_query <- forms_con_merged %>% filter(editmessage == "Out of Range")

# get tab of variables that have default values 
default_values_continuous <- as.numeric(c("-5", "-8", "-7", "-9", "-6"))
out_range_default_value_continuous <- forms_con_merged %>% 
  group_by(varname) %>% 
  add_count(name = "n_total") %>%                    ## get the total count for each variable 
  filter(editmessage == "Out of Range") %>%          ## only look at the variables that do not include default values in the resposne options
  group_by(form, varname, response,n_total) %>%      ## group by variable name, response, total, and form
  count(name ="n_response") %>%                      ## count the number each specific response is reported 
  mutate(pcnt_total = (n_response/n_total)*100) %>%  ## get percentange each reponse 
  filter(response %in% default_values_continuous)    ## exclude to only the default values 

# remove default values from query, but will review tabulation at end of script 
forms_con_query <- forms_con_query %>% filter(!(response %in% default_values_continuous))

## only keep the first 7 columns 
forms_con_query <- forms_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
ConRangeQuery <- forms_con_query


# remove fetal biometry variables from this query since we checked them earlier 
ConRangeQuery <- ConRangeQuery %>% filter(!(varname %in% fetal_biometry_vars))

# remove MNH25 variables -- we will check these later 
ConRangeQuery <- ConRangeQuery %>% filter(form != "MNH25")

# update naming
names(ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 

if (nrow(ConRangeQuery)>=1){
  
  ConRangeQuery = cbind(QueryID = NA, 
                        UploadDate = UploadDate, 
                        #MomID = "NA", PregID = "NA",
                        #VisitDate = "NA", 
                        ConRangeQuery, 
                        #`Variable Name` = "NA",
                        FieldType = "Number", 
                        EditType = "Out of Range", 
                        DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(ConRangeQuery)>=1){
  ConRangeQuery$`Variable Value` = as.character(ConRangeQuery$`Variable Value`)
}
#*****************************************************************************
#*MNH25 Variable Queries 
#*****************************************************************************
# Set MNH25 form
mnh25_to_exclude <- c("MNH25_Ghana", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan", "MNH25_India") # make a vector of all MNH25 forms 
mnh25_to_exclude <- mnh25_to_exclude[!grepl(paste0(site), mnh25_to_exclude)] # exclude the form of the site we are currently running. We will this to filter out all the forms we DONT need from the data dictionary

varNames_sheet <- varNames_sheet %>%
  filter(!(form %in% mnh25_to_exclude))

if (site=="Kenya"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Kenya") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #VisitDate = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Kenya")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site=="Pakistan"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Pakistan") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #VisitDate = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Pakistan")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site=="Ghana"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Ghana") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #VisitDate = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Ghana")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site=="Zambia"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Zambia") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #VisitDate = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Zambia")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}


if (site=="India"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_India") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #VisitDate = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_India")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}


if (nrow(M25_OutRangeNumericQuery)>=1){
  M25_OutRangeNumericQuery$`Variable Value` = as.character(M25_OutRangeNumericQuery$`Variable Value`)
}
#*****************************************************************************
#* Remove all emply dataframe 
#* this means that the only data frames left are the ones with true queries   
#*****************************************************************************
# 
## create a function that returns a logical value
isEmpty <- function(x) {
  is.data.frame(x) && nrow(x) == 0L
}
## apply it over the environment
empty <- unlist(eapply(.GlobalEnv, isEmpty))

## remove the empties
rm(list = names(empty)[empty])

## export 
## bind forms 
out <- rbindlist(mget(ls(pattern = "*Query")))

# combine form/edit type var -- will have to do for each of forms that have duplicates 
out <- add_column(out, Form_Edit_Type=paste(out$Form,"_",out$EditType))

# add visit type column 
out = add_column(out, VisitType = NA , .after = "InfantID")

OutRangeCheck_query <- out

## assign queryid -- 
# edit type id for out of range is 05 
# edit type id for invalid response option is 09

OutRangeCheck_query <- OutRangeCheck_query %>% 
  mutate(QueryID = ifelse(EditType == "Invalid Response Option", paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "05"), 
                          ifelse(EditType == "Out of Range", paste0(Form, "_", VisitDate, "_",MomID, "_", `Variable Name`, "_", `Variable Value`, "_", "09"), NA)
  ))

# export out of range queries 
save(OutRangeCheck_query, file = "queries/OutRangeCheck_query.rda")

## the follow tables show the % of responses that were default values that were NOT already included on the answer option
# example: Q1 has response options of 1,0,88 but a 77 was recorded -- this table will report what were the % of responses that were 77
# if these values are high, then there might be a skip pattern issue or the site has implemented their own default value system 

view(invalid_response_default_value)
view(out_range_default_value_continuous)


## the follow tables show the % of responses that were default values that were NOT already included on the answer option
# example: Q1 has response options of 1,0,88 but a 77 was recorded -- this table will report what were the % of responses that were 77
# if these values are high, then there might be a skip pattern issue or the site has implemented their own default value system 


#This is to extract the Create more information for the High frequency Variables categorical and to include the response, count and expected response range 
# Step 0: Get the real queries out

invalid_response_high_freq_query <- invalid_response_categorical

invalid_response_high_freq_query_filtered <- invalid_response_high_freq_query %>%
  filter(editmessage == "Invalid Response Option")

invalid_response_high_freq_query_filter <- invalid_response_high_freq_query_filtered %>%
  filter(!(response %in% c("77", "55", "88", "99")))


#Step One: Get the count and group by response
summary_numeric_1 <- invalid_response_high_freq_query_filter %>%  group_by(form, varname, response) %>%
  summarize(count = n())

#Step Two: Get the common response range
common_data <- invalid_response_high_freq_query_filter %>%
  group_by(response, response_range, form, varname) %>%
  summarize(response_range = unique(response_range))

#Join the two data frames
OutRange_Invalid_Query_Summary <- inner_join(summary_numeric_1, common_data, by = c("form", "varname", "response")) %>% 
  cbind(min = "*", max = "*", DefaultValue = "77, Not Applicable; 55, Missing Data Point", EditType ="Invalid Response") 

OutRange_Invalid_Query_Summary <- OutRange_Invalid_Query_Summary %>%
  mutate(Recommendations = ifelse(response %in% c("-7", "N/A", "n/a", "na", "NA", "Na"),
                                  "Change default value to 77 if not applicable",
                                  ifelse(response %in% c("-5"),
                                         "Change default value to 55 if data point is missing",
                                         "Input valid data within the Response Range Options")))

#For Continuous Variables
#Step One: Get the count and group by response

summary_cont_1 <- forms_con_query %>% 
  group_by(varname, form, response) %>% 
  summarize(count = n())


#Step Two: Get the common response range
common_data_cont <- forms_con_merged %>%
  group_by(min, max, response, form, varname) %>%
  summarize(min = unique(min)) %>%
  cbind(DefaultValue = " -5, Missing Data point; -7,Not Applicable; -9,Dont Know -6,Refused to Answer")

# Join the two data frames
OutRange_Cont_Query_Summary <- inner_join(summary_cont_1, common_data_cont, by = 
                                            c("form", "varname", "response")) %>% cbind(response_range = "*") %>%  
  subset((response < min) |  (response > max))


# Convert response column in OutRange_Cont_Query_Summary to character
OutRange_Cont_Query_Summary_1 <- OutRange_Cont_Query_Summary %>% 
  mutate(response = as.character(response)) %>% 
  mutate(min = as.character(min))  %>%  
  mutate(max = as.character(max)) %>% 
  add_column (EditType="Out of Range")

OutRange_Cont_Query_Summary_1 <- OutRange_Cont_Query_Summary_1 %>%
  mutate(Recommendations = ifelse(response == 77, 
                                  "Change default value to -7, if not applicable", 
                                  "Input valid data within the minimum and max range" ))


# Inner join the two data frames
High_Freq_Invalid_Query <- full_join(OutRange_Cont_Query_Summary_1, OutRange_Invalid_Query_Summary) %>%  filter(count > 40)

High_Freq_Invalid_Query <- add_column(High_Freq_Invalid_Query, 'Form and Edit Type'=paste
                                      (High_Freq_Invalid_Query$form, "_", High_Freq_Invalid_Query$`varname`,"_", 
                                        High_Freq_Invalid_Query$EditType, sep = ""))

save(High_Freq_Invalid_Query, file = "queries/High_Freq_Invalid_Query.rda")


### extra -- pulling lab queries 
lab_queries <- OutRangeCheck_query %>%
  filter(Form == "MNH08") %>%
  select(Form, 'Variable Name', 'Variable Value') %>%
  group_by(`Variable Name`) %>%
  mutate(mean = round(mean(as.numeric(`Variable Value`)),1),
         sd = round(sd(as.numeric(`Variable Value`)),1),
         min = min(as.numeric(`Variable Value`)),
         max = max(as.numeric(`Variable Value`))
         ) %>%
  select(-`Variable Value`) %>%
  group_by(`Variable Name`) %>%
  add_count(name = "n_total") %>%
  distinct()

# merge in dd
varNames_sheet <- read_excel("~/PRiSMAv2Data/Queries/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
varNames_sheet <- varNames_sheet %>%filter(Form == "MNH08") %>%  select(`Variable Name`,`Minimum Value`,`Maximum Value` )
names(varNames_sheet) = c("Variable Name", "DD Min", "DD Max")

lab_queries <- left_join(lab_queries, varNames_sheet, by = "Variable Name")

lab_queries <- lab_queries %>% filter(mean != -7)

#setwd("D:/Users/stacie.loisate/Documents/PRiSMAv2Data/Lab")
## export all to a new excel sheet
xl_lst <- list('site' = lab_queries)
write.xlsx(xl_lst, file = paste("lab_report_high_freq_", UploadDate, ".xlsx", sep = ""))


