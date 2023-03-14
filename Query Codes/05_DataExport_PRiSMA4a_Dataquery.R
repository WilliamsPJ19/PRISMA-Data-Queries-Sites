#*****************************************************************************
#*QUERY #5 -- Generate Query Report 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 13 March 2023

#*Input: all .rds files 
#*Function: merge all queries together and assign query ID 
#*Output: excel sheet with a full query report 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Set site 
#* 2. Set working directory to site-specific folder  

## SITE SPECIFIC UPDATES: 
# Sites to set their site name (line 40)

#* Once the previous lines of code are updated, you can start to run the script 

#* NOTES:
#* If you make any updates to the query report in excel AFTER you exported it, you will need to import it again to remake the summary table (see code below)

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
library(openxlsx)

## UPDATE EACH RUN ## 
## 1. Set site 
site = "PRISMA-Kenya"

## UPDATE EACH RUN ## 
## 2. Set working directory to site-specific folder -- QUERIES folder
setwd("~/Documents/PRiSMAv2Data/Kenya/2023-02-03/queries")

## import all rda files 
rda_files = list.files(pattern="*.rda")
walk(rda_files, ~ load(.x, .GlobalEnv))

## bind forms 
report <- rbindlist(mget(ls(pattern = "*query")))

# assign Query ID 
report <- report %>% mutate(
  EditTypeID = case_when(
    EditType == "Duplicate ID" ~ "01", 
    EditType == "Extra Variable" ~ "02", 
    EditType == "Missing Variable" ~ "03", 
    EditType == "MOMID Missing Enrollment" ~ "04", 
    EditType == "Out of Range" ~ "05", 
    EditType == "Visit Type Error" ~ "06",
  ),
  QueryID = case_when(
    EditTypeID %in% c("02", "03") ~ paste0(Form, "_", `Variable Name`, "_", EditTypeID),
    Form %in% c("MNH00", "MNH01", "MNH02") ~ paste0(Form, "_", DateFormCompleted, "_", ScrnID, "_", EditTypeID), 
    TRUE ~ paste0(Form, "_", DateFormCompleted, "_", MomID, "_", EditTypeID)
  )) %>% select(-EditTypeID)

# st=format(Sys.time(), "%Y-%m-%d")
# write.xlsx(query_out, file = paste(site, "_query_report_",st,"_draft", ".xlsx", sep = ""))

#*****************************************************************************
#* Making summary tables
#*****************************************************************************
## add in new column that had form_varname_edittype
report = add_column(report, VarFormEdit=paste(report$Form, "_", report$`Variable Name`,"_", report$EditType, sep = ""))

## add in new columns for RemoveEdit variable and notes  
report = add_column(report, RemoveEdit = "", Notes = "") 

## get frequency table that shows the number of queries for each form-varname-edit type 
  ## main function is to look at high frequency variables 
table_freq_FmNmEd <- report %>% 
  group_by(VarFormEdit) %>% 
  count(name ="Frequency") %>% 
  rename("Form - Variable Name - Edit Type" = "VarFormEdit") %>% 
  filter(Frequency > 100)

## STOP HERE TO CHECK FOR HIGH FREQUENCY VARIABLES 

## here we can remove any variables that pulled a lot of queries 
  # we can note them but then remove from the query report tab (still include them in the last Form_Varname_Edit tab)
  # Examples here might include a specific lab that is pulling 150 queries - this might be a range or unit issue - just bring up in email

high_freq_to_exclude <- report %>% 
  group_by(`Variable Name`, VarFormEdit) %>% 
  count(name ="Frequency") %>% 
  filter(Frequency > 100) %>%     ## let's say we want to remove all instances that happened >100 times 
  pull(`Variable Name`)           ## get variable names to exclude 

## remove these high frequency variables from the report 
report <- report %>% filter(!(`Variable Name` %in% high_freq_to_exclude))

## get frequency table that shows the number of queries for each form-edit type 
table_freq_FmEd <- report %>% 
  group_by(Form_Edit_Type) %>% 
  count(name ="Frequency") %>% 
  rename("Form and Edit Type" = "Form_Edit_Type")

## export all to a new excel sheet 
st=format(Sys.time(), "%Y-%m-%d")
xl_lst <- list('Summary' = table_freq_FmEd, 'Query Report' = report, 'Form_Varname_EditType' = table_freq_FmNmEd)
write.xlsx(xl_lst, file = paste(site, "_query_report_", st, ".xlsx", sep = ""))

### if you make changes to the query report after you have exported it, you may need to update the summary table 

# # Step 1. read in query report 
# report_updated <- read_excel("PRISMA-Kenya_query_report_TEST2023-02-14.xlsx", sheet = "Query Report")
# 
# # Step 2. make new summary table 
# ## get frequency table that shows the number of queries for each form-edit type 
# table_freq_FmEd <- report_updated %>% 
#   group_by(Form_Edit_Type) %>% 
#   count(name ="Frequency") %>% 
#   rename("Form and Edit Type" = "Form_Edit_Type")
# 
# # Step 3. add table with high frequency values back into the report 
# table_freq_FmNmEd <- report %>% 
#   group_by(VarFormEdit) %>% 
#   count(name ="Frequency") %>% 
#   rename("Form - Variable Name - Edit Type" = "VarFormEdit") %>% 
#   filter(Frequency > 100)
# 
# # Step 4. Export updated report 
# ## export all to a new excel sheet 
# st=format(Sys.time(), "%Y-%m-%d")
# xl_lst <- list('Summary' = table_freq_FmEd, 'Query Report' = report_updated, 'Form_Varname_EditType' = table_freq_FmNmEd)
# write.xlsx(xl_lst, file = paste(site, "_query_report_",st, ".xlsx", sep = ""))


