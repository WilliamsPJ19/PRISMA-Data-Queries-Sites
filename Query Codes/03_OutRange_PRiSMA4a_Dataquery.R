#*****************************************************************************
#*QUERY #3 -- CHECK FOR OUT OF RANGE VALUES 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 13 March 2023

#*Input: Long data 
#*Function: check for any out of range values  
#*Output: .rda file with all out of range values 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Update "UploadDate" 
#* 2. Set Site  
#* 3. Set working directory to site-specific folder 
#* 4. Load in long data 
#* 5. Set MNH25 form 

## SITE SPECIFIC UPDATES: 
# Sites to set their specific MNH25 form (line 82) - make sure your site is the only site NOT hashtagged  

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
# 1. Update "UploadDate" 
UploadDate = "2023-02-10"

## UPDATE EACH RUN ## 
# 2. Set Site  
site = "Kenya"

# make reorder function to use for later 
##rearrange columns 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}

#*****************************************************************************
#* load data
#*****************************************************************************
## UPDATE EACH RUN ## 
## 3. Set working directory to site-specific folder 
setwd("~/PRiSMAv2Data/Kenya/2023-02-10/")

## UPDATE EACH RUN ## 
## 4. Load in long data 
load("~/PRiSMAv2Data/Kenya/2023-02-10/data/2023-02-10_long.RData")

# ## update for zambia 
# out <- data_long
# out$DateFormCompleted = dmy(out$DateFormCompleted)
# data_long <- out %>% filter(DateFormCompleted >="2022-12-01")
#*****************************************************************************
#*import data dictionary 
#*****************************************************************************
#* create excel work book 
varNames_sheet <- read_excel("~/PRiSMAv2Data/Queries/PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_Queries.xlsx")
varNames_sheet <- varNames_sheet %>% dplyr::select(Form, `Variable Name`, `Response Options`, `Query Category`,`ResponseRange`,`DefaultValue`, `Field Type (Date, Time, Number, Text)`, `Minimum Value`, `Maximum Value`)
varNames_sheet$`Variable Name` = toupper(varNames_sheet$`Variable Name`)

## UPDATE EACH RUN ## 
# 5. Set MNH25 form -- place a hashtag in front of the countries you are NOT currently running 
varNames_sheet <- varNames_sheet %>% rename("varname" = `Variable Name`, "form" = "Form") %>%
  filter(#form == "MNH25_Ghana", 
         form == "MNH25_Kenya"
         #form == "MNH25_Zambia", 
         #form == "MNH25_Pakistan", 
         #form == "MNH25_India"
         )
#*****************************************************************************
#*****************************************************************************
#* Fetal Biometry Checks 
#******************************************************************************
#* CAL_GA_WKS_AGE_FTS1 -- estimated GA in days by ACOG 
#* FL_PERES_01_FTS1; FL_PERES_MEAN_FTS1 
#* AC_PERES_01_FTS1; AC_PERES_MEAN_FTS1
#* HC_PERES_01_FTS1; HC_PERES_MEAN_FTS1
#* BPD_PERES_01_FTS1; BPD_PERES_MEAN_FTS1
fetalRange_sheet <- read_excel("~/PRiSMAv2Data/Queries/fetal_biometry_range.xlsx")

fetal_biometry_vars = c("CAL_GA_WKS_AGE_FTS1", "CAL_GA_WKS_AGE_FTS1", "CAL_GA_WKS_AGE_FTS1", "CAL_GA_WKS_AGE_FTS1", ## get GA 
                        "FL_PERES_01_FTS1", "FL_PERED_01_FTS2", "FL_PERED_01_FTS3", "FL_PERED_01_FTS4", ## first measurement of FL 
                        "FL_PERES_02_FTS1", "FL_PERED_02_FTS2", "FL_PERED_02_FTS3", "FL_PERED_02_FTS4", ## second measurement of FL
                        "FL_PERES_MEAN_FTS1", "FL_PERES_MEAN_FTS2", "FL_PERES_MEAN_FTS3", "FL_PERES_MEAN_FTS4",   ## FL mean 
                        "AC_PERES_01_FTS1", "AC_PERED_01_FTS2", "AC_PERED_01_FTS3", "AC_PERED_01_FTS4", ## first measurement of AC 
                        "AC_PERES_02_FTS1", "AC_PERED_02_FTS2", "AC_PERED_02_FTS3", "AC_PERED_02_FTS4", ## second measurement of AC 
                        "AC_PERES_MEAN_FTS1", "AC_PERES_MEAN_FTS2", "AC_PERES_MEAN_FTS3", "AC_PERES_MEAN_FTS4", ## AC mean
                        "HC_PERES_01_FTS1", "HC_PERED_01_FTS2", "HC_PERED_01_FTS3", "HC_PERED_01_FTS4", ## first measurement of HC
                        "HC_PERES_02_FTS1", "HC_PERED_02_FTS2", "HC_PERED_02_FTS3", "HC_PERED_02_FTS4", ## second measurement of HC
                        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4", ## HC mean 
                        "BPD_PERES_01_FTS1", "BPD_PERED_01_FTS2", "BPD_PERED_01_FTS3", "BPD_PERED_01_FTS4", ## first measurement of bpd 
                        "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS2", "BPD_PERED_02_FTS3", "BPD_PERED_02_FTS4", ## second measurement of bpd 
                        "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4") ## BPD mean 

# extract fetal biometry  variables from the data 
forms_fetal_biomery <- data_long %>% filter(varname %in% fetal_biometry_vars)

## isolate GA 
GA_by_id <- forms_fetal_biomery %>% filter(varname == "CAL_GA_WKS_AGE_FTS1" | varname ==  "CAL_GA_WKS_AGE_FTS2" | 
                                             varname == "CAL_GA_WKS_AGE_FTS3" | varname == "CAL_GA_WKS_AGE_FTS4")

GA_by_id=GA_by_id[,-7] ## remove variable name column
names(GA_by_id) = c("SCRNID", "MOMID", "PREGID", "INFANTID", "DateFormCompleted", "form", "GestAge_Wks") ## rename columns for merging 

## remove GA from original data 
forms_fetal_biomery_noGA <- forms_fetal_biomery %>% filter(varname != "CAL_GA_WKS_AGE_FTS1" | varname !=  "CAL_GA_WKS_AGE_FTS2" | 
                                                             varname != "CAL_GA_WKS_AGE_FTS3" | varname != "CAL_GA_WKS_AGE_FTS4")
## merge back together 
forms_fetal_biomery_GA <- merge(forms_fetal_biomery_noGA, GA_by_id, by = c("SCRNID", "MOMID", "PREGID", "INFANTID", "DateFormCompleted", "form"))

## convert weeks to days 
forms_fetal_biomery_GA$GestAge_Days = as.numeric(forms_fetal_biomery_GA$GestAge_Wks)*7

## merge fetal biometry variables and data 
forms_fetal_biomery_ranges <- merge(forms_fetal_biomery_GA, fetalRange_sheet, by = "GestAge_Days")

# query BPD 
bpd <- c("BPD_PERES_01_FTS1", "BPD_PERED_01_FTS2", "BPD_PERED_01_FTS3", "BPD_PERED_01_FTS4", ## first measurement of bpd 
         "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS2", "BPD_PERED_02_FTS3", "BPD_PERED_02_FTS4", ## second measurement of bpd 
         "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4")## BPD mean 

forms_fetal_biomery_bpd <- forms_fetal_biomery_ranges %>% filter(varname %in% bpd, response !=-7)
forms_fetal_biomery_bpd$outrange <- ifelse((forms_fetal_biomery_bpd$response >= forms_fetal_biomery_bpd$bpd_min & forms_fetal_biomery_bpd$response <= forms_fetal_biomery_bpd$bpd_max) | forms_fetal_biomery_bpd$response!= "-7", "", forms_fetal_biomery_bpd$response)

# query HC
hc <- c("HC_PERES_01_FTS1", "HC_PERED_01_FTS2", "HC_PERED_01_FTS3", "HC_PERED_01_FTS4", ## first measurement of HC
        "HC_PERES_02_FTS1", "HC_PERED_02_FTS2", "HC_PERED_02_FTS3", "HC_PERED_02_FTS4", ## second measurement of HC
        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4") ## HC mean 

forms_fetal_biomery_hc <- forms_fetal_biomery_ranges %>% filter(varname %in% hc, response !=-7)
forms_fetal_biomery_hc$outrange <- ifelse((forms_fetal_biomery_hc$response >= forms_fetal_biomery_hc$hc_min & forms_fetal_biomery_hc$response <= forms_fetal_biomery_hc$hc_max) | forms_fetal_biomery_hc$response!= "-7", "", forms_fetal_biomery_hc$response)

# query AC 
ac <- c("AC_PERES_01_FTS1", "AC_PERED_01_FTS2", "AC_PERED_01_FTS3", "AC_PERED_01_FTS4", ## first measurement of AC 
        "AC_PERES_02_FTS1", "AC_PERED_02_FTS2", "AC_PERED_02_FTS3", "AC_PERED_02_FTS4", ## second measurement of AC 
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
forms_fetal_biometry_query = forms_fetal_biometry_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
FetalBioRangeQuery <- forms_fetal_biometry_query

# update naming
names(FetalBioRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(FetalBioRangeQuery)>=1){
  FetalBioRangeQuery = cbind(QueryID = NA, 
                             UploadDate = UploadDate, 
                             #MomID = "NA", PregID = "NA",
                             #DateFormCompleted = "NA", 
                             FetalBioRangeQuery, 
                             #`Variable Name` = "NA",
                             FieldType = "Number", 
                             EditType = "Out of Range", 
                             DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}


#*****************************************************************************
#*Categorical Variable Queries 
#*****************************************************************************

## merge data dictionary and site data  
out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
out_range_numeric <- out_range_merge %>% filter(`Query Category` == "Number")

# make the response range a vector 
out_range_numeric$response_range = as.vector(c(out_range_numeric$`ResponseRange`))

# run the query
out_range_numeric$in_range <- apply(out_range_numeric,1, function(x){
  grepl(x["response"], x["response_range"])
})

# make edit message 
out_range_numeric$editmessage <- ifelse(out_range_numeric$in_range == "FALSE", "Out of Range", "No Error") 

# filter out those that are out of range 
out_range_numeric_query <- out_range_numeric %>% filter(editmessage == "Out of Range")

# get tab of variables that have default values 
default_values <- c("55", "88", "77", "99", "66")
out_range_default_value <- out_range_numeric %>% 
  group_by(varname) %>% 
  add_count(name = "n_total") %>%                    ## get the total count for each variable 
  filter(editmessage == "Out of Range") %>%          ## only look at the variables that do not include default values in the resposne options
  group_by(form, varname, response,n_total) %>%      ## group by variable name, response, total, and form
  count(name ="n_response") %>%                      ## count the number each specific response is reported 
  mutate(pcnt_total = (n_response/n_total)*100) %>%  ## get percentange each reponse 
  filter(response %in% default_values)               ## exclude to only the default values 

# remove default values for query but will review at the end of script 
out_range_numeric_query <- out_range_numeric_query %>% filter(!(response %in% default_values)) 

# remove MNH25 variables -- we will check these later 
out_range_numeric_query <- out_range_numeric_query %>% filter(form != "MNH25")

## only keep the first 7 columns 
OutRangeNumericQuery = out_range_numeric_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)

# update naming
names(OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(OutRangeNumericQuery)>=1){
  OutRangeNumericQuery = cbind(QueryID = NA, 
                             UploadDate = UploadDate, 
                             #MomID = "NA", PregID = "NA",
                             #DateFormCompleted = "NA", 
                             OutRangeNumericQuery, 
                             #`Variable Name` = "NA",
                             FieldType = "Number", 
                             EditType = "Out of Range", 
                             DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

#*****************************************************************************
#*Continuous 
#*****************************************************************************
# extract continuous variables from the data dictionary 
requested_varNames_out <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (!is.na(`Minimum Value`) | !is.na(`Maximum Value`)), 
                                                    !(`varname` %in% fetal_biometry_vars)) 
form_num = toupper(form_num)
requested_varNames_out_var <- requested_varNames_out %>% filter(form %in% form_num) %>% 
  #rename("varname" = `Variable Name`) %>% 
  pull(varname)

# extract the continuous variables that don't have min max values -- save for later 
requested_varNames_Nominmax <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (is.na(`Minimum Value`) | is.na(`Maximum Value`)))

# extract continuous variables from the data 
forms_con <- data_long %>% filter(varname %in% requested_varNames_out_var)

# merge together the min/max value columns 
requested_varNames_out_w_MinMax <- requested_varNames_out %>% dplyr::select(form, `varname`, `Minimum Value`, `Maximum Value`, `DefaultValue`)
names(requested_varNames_out_w_MinMax) = c("form", "varname", "min", "max", "DefaultValue")
forms_con_merged <- merge(forms_con, requested_varNames_out_w_MinMax, by = c("form", "varname"))

# make sure response options, min, and max values are numeric 
forms_con_merged$response = as.numeric(forms_con_merged$response)
forms_con_merged$min = as.numeric(forms_con_merged$min)
forms_con_merged$max = as.numeric(forms_con_merged$max)
forms_con_merged$DefaultValue = as.numeric(forms_con_merged$DefaultValue)

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
forms_con_query <- forms_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
ConRangeQuery <- forms_con_query


# remove fetal biometry variables from this query since we checked them earlier 
ConRangeQuery <- ConRangeQuery %>% filter(!(varname %in% fetal_biometry_vars))

# remove MNH25 variables -- we will check these later 
ConRangeQuery <- ConRangeQuery %>% filter(form != "MNH25")

# update naming
names(ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")

## add additional columns 

if (nrow(ConRangeQuery)>=1){
  
  ConRangeQuery = cbind(QueryID = NA, 
                        UploadDate = UploadDate, 
                        #MomID = "NA", PregID = "NA",
                        #DateFormCompleted = "NA", 
                        ConRangeQuery, 
                        #`Variable Name` = "NA",
                        FieldType = "Number", 
                        EditType = "Out of Range", 
                        DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

#*****************************************************************************
#*MNH25 Variable Queries 
#*****************************************************************************
if (site=="Kenya"){

# extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Kenya") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    #rename("varname" = `Variable Name`) %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 13, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query

  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                          UploadDate = UploadDate, 
                          #MomID = "NA", PregID = "NA",
                          #DateFormCompleted = "NA", 
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
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                 UploadDate = UploadDate, 
                                 #MomID = "NA", PregID = "NA",
                                 #DateFormCompleted = "NA", 
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
    #rename("varname" = `Variable Name`) %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 13, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #DateFormCompleted = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = "varname")
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
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #DateFormCompleted = "NA", 
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
    #rename("varname" = `Variable Name`) %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 13, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #DateFormCompleted = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = "varname")
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
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #DateFormCompleted = "NA", 
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
    #rename("varname" = `Variable Name`) %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 13, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #DateFormCompleted = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = "varname")
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
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #DateFormCompleted = "NA", 
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
    #rename("varname" = `Variable Name`) %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 13, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range")
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,DateFormCompleted, form, varname, response) 
  M25_ConRangeQuery <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery)>=1){
    
    M25_ConRangeQuery = cbind(QueryID = NA, 
                              UploadDate = UploadDate, 
                              #MomID = "NA", PregID = "NA",
                              #DateFormCompleted = "NA", 
                              M25_ConRangeQuery, 
                              #`Variable Name` = "NA",
                              FieldType = "Number", 
                              EditType = "Out of Range", 
                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = "varname")
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
  M25_OutRangeNumericQuery = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery) = c("ScrnID","MomID", "PregID","InfantID", "DateFormCompleted", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery)>=1){
    M25_OutRangeNumericQuery = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #DateFormCompleted = "NA", 
                                     M25_OutRangeNumericQuery, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
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

# export out of range queries 
save(OutRangeCheck_query, file = "queries/OutRangeCheck_query.rda")

## the follow tables show the % of responses that were default values that were NOT already included on the answer option
  # example: Q1 has response options of 1,0,88 but a 77 was recorded -- this table will report what were the % of responses that were 77
  # if these values are high, then there might be a skip pattern issue or the site has implemented their own default value system 

view(out_range_default_value) 
view(out_range_default_value_continuous)
