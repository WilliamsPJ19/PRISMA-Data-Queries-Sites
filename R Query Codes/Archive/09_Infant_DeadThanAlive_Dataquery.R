#*****************************************************************************
#* QUERY #9 -- DEAD THAN ALIVE QUERY: INFANT
#* Input: Wide data (all raw .csv files) & Long data
#* Function:  Are there are new visits occurring after an infant has reported a death 
#* Output: .rda file with all InfantIDS that have a visit that occurs after the reported death
#* Logic: extract all visit dates into a column; if these dates occur after the reported death date, these are flagged for queries 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* Last updated: 06 March 2024

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for

#* Once the previous lines of code are updated, you can start to run the script 
#*****************************************************************************
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
UploadDate = "2024-03-08"
site = "India-SAS"

# 3. Set your main directory 
maindir <- paste0("~/PRiSMAv2Data/", site,"/", UploadDate, sep = "")
#*****************************************************************************
#* load data
#*****************************************************************************
## Load in long data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_long.Rdata", sep = "")) 

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 


## RUN THE FOLLOWING CODE TO ENSURE THE SITE HAS THE DATA REQUIRED TO RUN THE CODE. 
  ## if a site has all the forms needed for this query, the remainder of the code will run. if not, the code will stop
out_vars_needed_1 <- data_long %>% 
  filter(form %in% c("MNH13", "MNH20", "MNH24")) 

out_vars_needed_2 <- data_long %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) 

if (exists("mnh04")== TRUE & exists("mnh09")== TRUE & exists("mnh11")== TRUE & 
    dim(out_vars_needed_1)[1]>0 &  dim(out_vars_needed_2)[1]>0 ){
  
  print("All forms required to run this query exist.")
  
} else {
  
  stop("SITE does not have all data required to run this query - don't run.")
  
}
#*****************************************************************************
## Types: 
# liveborn dead than alive 
# stillborn then alive
# fetal loss then alive 

## fetal death
#* miscarriage or induced abortion: reported on MNH04 (no IPC forms required) - likely won't have infantid 
#* stillbirth: document loss on one of the following - MNH04, MNH09, MNH19 + NEED to fillout IPC forms, MNH28 required, MNH24 closeout is optional
#* infant death: documentd on MNH13 plus MNH20 if hospitalized plus MNH24
#*****************************************************************************
#* PULL IDS OF INFANTS
#*****************************************************************************
# pull all infantids from mnh09
delivered_infantids <- mnh09 %>% 
  select(MOMID, PREGID, contains("INFANTID_INF")) %>% 
  pivot_longer(cols = c(-MOMID, -PREGID), 
               names_to = "var",
               values_to = "INFANTID") %>% 
  filter(!INFANTID %in% c("n/a", "0", "77", "1907-07-07") , 
         !is.na(INFANTID)) %>% 
  group_by(INFANTID) %>% 
  distinct() %>% 
  select(-var)

#*****************************************************************************
#* Run query for Infant dead than alive
#* Protocol in the event of infant death:
  # must have MNH13 reported (INFANT_DTHDAT, INFANT_DTHTIM) 
  # if hospitalized, reported on MNH20 (DTHDAT, DTHTIM)
  # MNH24 closeout filled (DTHDAT, DTHTIM)

#* Query Logic:
  # 1. Extract all liveborn infants 
  # 2. Extract all infants with reported deaths in MNH13 OR MNH20 OR MNH24
  # 3. Generate data subset will provide the date and vital status for each visit among infants with a reported date of death
  # 4. Pull any visit dates that occur AFTER the reported date of death and vital status at the time of visit is "alive"
#*****************************************************************************
## extract infants with mnh13 (infant clinical status), mnh20 (infant hospitalizaiton), or mnh24 (closeout)
out_infants_dth_date <- data_long %>% 
  filter(form %in% c("MNH13", "MNH20", "MNH24")) %>% 
  filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
  filter(varname %in% c("INFANT_DTHDAT", "DTHDAT")) %>% 
  mutate(DEATH_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  filter(!DEATH_DATE %in% c(ymd("2007-07-07"), ymd("1907-07-07"))) %>% 
  rename("FORM_DTH_REPORTED" = "form") %>% 
  select(MOMID, PREGID, INFANTID, DEATH_DATE, FORM_DTH_REPORTED)

# Extract all infant IDs reporting a death from the data 
# this data subset will provide the date and vital status for each visit among infants with a reported date of death (generated above) 
all_infid_data <- data_long %>% filter(INFANTID %in% as.vector(out_infants_dth_date$INFANTID)) %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) %>% 
  mutate(VISIT_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(VisitDate =  ymd(parse_date_time(VisitDate, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  filter(!VISIT_DATE %in% c(ymd("2007-07-07"), ymd("1907-07-07"))) %>% 
  # generate a single variable that combines all infant vital status variables across forms 
  # mutate(INF_VITAL = case_when(
  #   str_detect(varname, "INF_VITAL") ~ response, 
  #   TRUE ~ NA
  # )) %>% 
  mutate(INF_VITAL = ifelse(str_detect(varname, "INF_VITAL"), response, NA)) %>%
  rename("FORM" = "form") %>% 
  # fill the vital status for each infant at a specified visit 
  group_by(INFANTID, FORM, VisitDate) %>% 
  fill(INF_VITAL, .direction = "up") %>% 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT"))  %>% 
  select(-response, -varname, -VISIT_DATE)
  

# merge infants with a reported death and all visit data
if (dim(out_infants_dth_date)[1]>0 & dim(all_infid_data)[1]>0){
  ## merge 
  inf_dead_then_alive <- left_join(out_infants_dth_date, all_infid_data, by = c("MOMID", "PREGID", "INFANTID"))
  
}

## generate query column 
if (exists("inf_dead_then_alive")){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
inf_dead_then_alive_query <- inf_dead_then_alive %>% 
          rename("VISIT_DATE" = "VisitDate")  %>% 
          ## if the visit date is after the death date AND the infant vital status at that visit is "1, alive" then query
          mutate(VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE >  DEATH_DATE & INF_VITAL == 1 ~ 1, 
                                            TRUE ~ 0)) %>% 
  ## extract the queries
  filter(VISIT_DATE_AFTER_DTH == 1) %>% 
  setcolorder(c("SCRNID", "MOMID", "PREGID", "INFANTID"))
}

#*****************************************************************************
## Formatting for the query report file
#*****************************************************************************
if (exists("inf_dead_then_alive_query")){
  
## QUESTION: should the mnh24 date of closeout be excluded because it is not an actual visit?

# rename data frame 
Infant_DeadThenAlive_query <- inf_dead_then_alive_query %>% 
  mutate(VisitType = NA) %>% 
  relocate("VisitType", .before = "VISIT_DATE") %>% 
  select(-DEATH_DATE, -FORM_DTH_REPORTED, -INF_VITAL, -VISIT_DATE_AFTER_DTH)

# update naming
names(Infant_DeadThenAlive_query) = c("ScrnID","MomID", "PregID","InfantID", "VisitType", "VisitDate", "Form")

if (dim(Infant_DeadThenAlive_query)[1] > 0){
  
  ## add additional columns 
  Infant_DeadThenAlive_query = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA",
                                     Infant_DeadThenAlive_query,
                                     `Variable Name` = NA, 
                                     `Variable Value` = NA,
                                     FieldType = "NA", 
                                     EditType = "Invalid visit following reported infant death", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  
  
  # combine form/edit type var 
  Infant_DeadThenAlive_query <- add_column(Infant_DeadThenAlive_query,Form_Edit_Type = paste(Infant_DeadThenAlive_query$Form,"_",Infant_DeadThenAlive_query$EditType))
  
  # assign queryid -- 
  # edit type id for Invalid Visit Following reported death is 11
  Infant_DeadThenAlive_query <- Infant_DeadThenAlive_query %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_", InfantID,  "_", "11"))
  
# Export data
save(Infant_DeadThenAlive_query, file = paste0(maindir,"/queries/Infant_DeadThenAlive_query.rda"))

}
}
#*****************************************************************************
## Generate "extra tab" to be included in the query report
#*****************************************************************************
## generate extra tab that will be include in the query report
if (exists("inf_dead_then_alive_query")){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
  inf_dead_than_alive_extra_tab <- inf_dead_then_alive_query %>%
    rename("INF_VITAL_STATUS_VISIT" = "INF_VITAL"
    )  %>% 
    mutate(TYPE_LOSS = "Infant Death",
           BIRTH_OUTCOME = "Live birth",
           INF_VITAL_STATUS_VISIT = case_when(INF_VITAL_STATUS_VISIT ==1 ~ "Alive", 
                                              INF_VITAL_STATUS_VISIT ==2 ~ "Died",
                                              INF_VITAL_STATUS_VISIT ==99 ~ "Don't know",
                                              TRUE ~ NA),
           VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE_AFTER_DTH ==1 ~ "Yes",
                                            VISIT_DATE_AFTER_DTH ==0 ~ "No",
                                            TRUE ~ NA),
           EDIT_TYPE = "Invalid visit following reported infant death") %>% 
    select(MOMID, PREGID, INFANTID, BIRTH_OUTCOME, TYPE_LOSS, DEATH_DATE, FORM_DTH_REPORTED, VISIT_DATE, FORM, INF_VITAL_STATUS_VISIT, VISIT_DATE_AFTER_DTH, EDIT_TYPE)
}

#*****************************************************************************
#* Stillbirth then alive
#* Protocol in the event of a stillbirth:
# Document loss on at least 1 of the forms 
  #* MNH04 (FETAL_LOSS_DSSTDAT), MNH09 (DELIV_DSSTDAT_INF1-4, BIRTH_DSTERM_INF1-4), MNH19 (no fetal loss date on this form)
  # IPC forms to be filled [MNH09 (DELIV_DSSTDAT_INF1-4, BIRTH_DSTERM_INF1-4), MNH11 (DTHDAT, DTHTIM)]
  # MNH24 is optional (DTHDAT, DTHTIM)

# if hospitalized, reported on MNH20 (DTHDAT, DTHTIM)
# MNH24 closeout filled (DTHDAT, DTHTIM)

#* Query Logic:
  # 1. Extract all reported stillbirths from MNH09 OR MNH11 OR MNH19
  # 3. Generate data subset will provide the date and vital status for each visit among stillbirths with a reported date of death
  # 4. Pull any visit dates that occur AFTER the reported date of death and vital status at the time of visit is "alive"
#*****************************************************************************
## need to convert MNH09 to long format to use the infant data within; infant variables are named with "_INFx" format - remove suffix and make long. Should have one row for each mom-baby pair 
if (exists("mnh09")==TRUE){
  
  ## first need to make m09 long format for each infant 
  m09_INF1 <- mnh09 %>% 
    rename("INFANTID" = "INFANTID_INF1") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF1')) %>% 
    mutate(INFANTID = as.character(INFANTID)) %>% 
    mutate_all(as.character)
  
  m09_INF2 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF2") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF2')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  m09_INF3 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF3") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF3')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  m09_INF4 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF4") %>%
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>% 
    rename_with(~str_remove(., '_INF4')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  ## bind all infants together
  mnh09_update <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
  
  ## remove INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4
  infantids_to_remove <- c("INFANTID_INF1", "INFANTID_INF2", "INFANTID_INF3", "INFANTID_INF4")
  mnh09_update <- mnh09_update %>% select(-any_of(infantids_to_remove))
  
  mnh09_long <- mnh09_update %>% bind_cols(SCRNID = NA) %>% 
    mutate(VisitDate = MAT_LD_OHOSTDAT) %>% 
    mutate(form = "MNH09",
           BIRTH_DSTERM = as.numeric(BIRTH_DSTERM)) %>% 
    select(MOMID, PREGID, INFANTID,MAT_LD_OHOSTDAT, DELIV_DSSTDAT, BIRTH_DSTERM) 
}

## extract infants with MNH04 (maternal clinical status), MNH09 (labor and delivery outcome), or MNH11 (infant delivery outcome)
out_stillbirth_dth_date <- mnh04 %>% 
  select("MOMID", "PREGID", "FETAL_LOSS_DSSTDAT", "PRG_DSDECOD", "FETAL_LOSS_DSDECOD") %>% 
  full_join(mnh09_long[c("MOMID", "PREGID", "INFANTID", "DELIV_DSSTDAT", "BIRTH_DSTERM")], by = c("MOMID", "PREGID")) %>% 
  full_join(mnh11[c("MOMID", "PREGID","INFANTID", "DTHDAT", "DTHTIM", "INF_DSTERM")], by = c("MOMID", "PREGID", "INFANTID")) %>%  
  ## convert date variables to date class
  mutate(DELIV_DSSTDAT =  ymd(parse_date_time(DELIV_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DTHDAT =  ymd(parse_date_time(DTHDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         FETAL_LOSS_DSSTDAT =  ymd(parse_date_time(FETAL_LOSS_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>%
  ## new variable if a loss was reported on MNH04 [PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3] or MNH09 [BIRTH_DSTERM== 2]
  mutate(LOSS_REPORTED = case_when(BIRTH_DSTERM== 2 | (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~ 1,
                                   TRUE ~ 0)) %>% 
  # filter for participants who have had a loss reported 
  filter(LOSS_REPORTED == 1) %>% 
  # replace any default value dates with NA
  mutate(DELIV_DSSTDAT = replace(DELIV_DSSTDAT, DELIV_DSSTDAT==ymd("1907-07-07"), NA),
         DTHDAT = replace(DTHDAT, DTHDAT==ymd("1907-07-07"), NA),
         FETAL_LOSS_DSSTDAT = replace(FETAL_LOSS_DSSTDAT, FETAL_LOSS_DSSTDAT==ymd("1907-07-07"), NA)) %>% 
  # generate new variable with date fetal loss was reported (either MNH09 or MNH04)
  mutate(DATE_LOSS_REPORTED = case_when(BIRTH_DSTERM == 2 & !is.na(DELIV_DSSTDAT) ~ DELIV_DSSTDAT, 
                                        (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~ FETAL_LOSS_DSSTDAT,
                                        TRUE ~ NA
  )) %>% 
  # generate new variable with what form fetal loss was reported on (either MNH09 or MNH04)
  mutate(FORM_DTH_REPORTED = case_when(BIRTH_DSTERM == 2 ~ "MNH09",
                                       (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~  "MNH04",
                                       TRUE ~ NA)) %>% 
  select(MOMID, PREGID, INFANTID, DATE_LOSS_REPORTED, FORM_DTH_REPORTED,BIRTH_DSTERM,INF_DSTERM) %>% 
  distinct()


# Extract all infant IDs reporting a death from the data 
# this data subset will provide the date and vital status for each visit among infants with a reported date of death (generated above) 

all_infid_data <- data_long %>% filter(INFANTID %in% as.vector(out_stillbirth_dth_date$INFANTID)) %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) %>% 
  # update date class for date variables
  mutate(VISIT_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(VisitDate =  ymd(parse_date_time(VisitDate, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  # generate a single variable that combines all infant vital status variables across forms 
  mutate(INF_VITAL = case_when(
    str_detect(varname, "INF_VITAL") ~ response, 
    TRUE ~ NA
  )) %>% 
  rename("FORM" = "form") %>% 
  group_by(INFANTID, FORM, VisitDate) %>% 
  # fill the vital status for each infant at a specified visit 
  fill(INF_VITAL, .direction = "up") %>% 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT"))  %>% 
  select(-response, -varname, -VISIT_DATE)


# merge reported stillbirths with all visit data
if (dim(out_stillbirth_dth_date)[1]>0 & dim(all_infid_data)[1]>0){
  stillbirth_then_alive <- full_join(out_stillbirth_dth_date, all_infid_data, by = c("MOMID", "PREGID", "INFANTID"))
} 


## generate query column
if (exists("stillbirth_then_alive")){
  stillbirth_then_alive_query <- stillbirth_then_alive %>% 
    rename("VISIT_DATE" = "VisitDate")  %>% 
    ## if the visit date is after the death date AND the infant vital status at that visit is "1, alive" then query
    mutate(DATE_AFTER_DTH = case_when(VISIT_DATE >  DATE_LOSS_REPORTED & INF_VITAL == 1 ~ 1, 
                                      TRUE ~ 0)) %>% 
    ## filter and format
    filter(DATE_AFTER_DTH == 1) %>% 
    setcolorder(c("SCRNID", "MOMID", "PREGID", "INFANTID"))
}

# Question: the logic is if the visit date is AFTER the death date AND the vital status at the time of visit is alive, then we are going to query 
# if the visit date is after the death date AND the vital status at the time of visit is dead then that is not a query -- is this correct?
#*****************************************************************************
## Formatting for the query report file
#*****************************************************************************
if (exists("stillbirth_then_alive_query")){

  # rename data frame 
  StillbirthThenAlive_query <- stillbirth_then_alive_query %>% 
    mutate(VisitType = NA) %>% 
    relocate("VisitType", .before = "VISIT_DATE") %>% 
    select(-DATE_LOSS_REPORTED, -FORM_DTH_REPORTED, -BIRTH_DSTERM,-INF_DSTERM, -INF_VITAL, -DATE_AFTER_DTH)
  
  # update naming
  names(StillbirthThenAlive_query) = c("ScrnID","MomID", "PregID","InfantID", "VisitType", "VisitDate", "Form")
  
  if (dim(StillbirthThenAlive_query)[1] > 0){
    
    ## add additional columns 
    StillbirthThenAlive_query = cbind(QueryID = NA, 
                                       UploadDate = UploadDate, 
                                       #MomID = "NA", PregID = "NA",
                                       #VisitDate = "NA", 
                                       StillbirthThenAlive_query, 
                                      `Variable Name` = NA, 
                                      `Variable Value` = NA,
                                       FieldType = "NA", 
                                       EditType = "Invalid visit following reported stillbirth", 
                                       DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    
    
    
    # combine form/edit type var 
    StillbirthThenAlive_query <- add_column(StillbirthThenAlive_query,Form_Edit_Type = paste(StillbirthThenAlive_query$Form,"_",StillbirthThenAlive_query$EditType))
    
    ## assign queryid -- 
    # edit type id for Invalid Visit Following reported death is 11
    StillbirthThenAlive_query <- StillbirthThenAlive_query %>% 
      mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_", InfantID,  "_", "11"))
    
    # Export data
    save(StillbirthThenAlive_query, file = paste0(maindir,"/queries/StillbirthThenAlive_query.rda"))
    
  }
}

#*****************************************************************************
## Generate "extra tab" to be included in the query report
#*****************************************************************************

## generate extra tab that will be include in the query report
if (dim(stillbirth_then_alive_query)[1]>0){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
  stillbirth_than_alive_extra_tab <- stillbirth_then_alive_query %>%
    rename("INF_VITAL_STATUS_VISIT" = "INF_VITAL",
           "VISIT_DATE_AFTER_DTH" = "DATE_AFTER_DTH",
           "DEATH_DATE" = "DATE_LOSS_REPORTED"
    )  %>% 
    mutate(TYPE_LOSS = "Stillbirth",
           BIRTH_OUTCOME = case_when(BIRTH_DSTERM ==1 ~ "Live birth",
                                     BIRTH_DSTERM ==2 ~ "Fetal loss",
                                     TRUE ~ NA),
           INF_VITAL_STATUS_VISIT = case_when(INF_VITAL_STATUS_VISIT ==1 ~ "Alive", 
                                              INF_VITAL_STATUS_VISIT ==2 ~ "Died",
                                              INF_VITAL_STATUS_VISIT ==99 ~ "Don't know",
                                              TRUE ~ NA),
           VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE_AFTER_DTH ==1 ~ "Yes",
                                            VISIT_DATE_AFTER_DTH ==0 ~ "No",
                                            TRUE ~ NA),
           EDIT_TYPE = "Invalid visit following reported stillbirth") %>% 
    select(MOMID, PREGID, INFANTID, BIRTH_OUTCOME, TYPE_LOSS, DEATH_DATE, FORM_DTH_REPORTED, VISIT_DATE, FORM, INF_VITAL_STATUS_VISIT, VISIT_DATE_AFTER_DTH, EDIT_TYPE)
}

#*****************************************************************************
## Bind & export infant dead then alive and stillbirth then alive "extra tabs" 
#*****************************************************************************

if (exists("inf_dead_than_alive_extra_tab")== TRUE & exists("stillbirth_than_alive_extra_tab")== TRUE){
  
  # bind infant and stillbirth queries
  inf_dead_than_alive_extra_tab <- bind_rows(inf_dead_than_alive_extra_tab, stillbirth_than_alive_extra_tab) %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
} else if (exists("inf_dead_than_alive_extra_tab")== TRUE & exists("stillbirth_than_alive_extra_tab")== FALSE) {
  
  inf_dead_than_alive_extra_tab <- inf_dead_than_alive_extra_tab %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
} else if (exists("inf_dead_than_alive_extra_tab")== FALSE & exists("stillbirth_than_alive_extra_tab")== TRUE) {
  
  inf_dead_than_alive_extra_tab <- stillbirth_than_alive_extra_tab %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
}




