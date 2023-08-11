#*****************************************************************************
#*QUERY #6 -- Generate Query Report 
#* Written by: Stacie Loisate & Xiaoyan Hu & Precious Williams
#* Last updated: 19 MAY 2023
# Line 82. Updated QueryID to include the variable name for out of range values
# Line 44. Update query id export to include new query for invalid ranges
# Lines 153-173. Added code to remove any "non-queries" based on site's feedback

#*Input: all .rds files 
#*Function: merge all queries together and assign query ID 
#*Output: excel sheet with a full query report 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Set site 
#* 2. Set working directory to site-specific folder  
#* 3. Set previous upload date
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
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "Site" variable to the site you are running the query for 
# 3. Set previous upload date (we will need this to import the previous query report)
UploadDate = "2023-05-26"
site = "Kenya"
PrevUploadDate = "2023-05-19"

## UPDATE EACH RUN ## 
## 3. Set site label
site_label = "PRISMA-Kenya"

## Set working directory to site-specific folder -- QUERIES folder
setwd(paste0("~/PRiSMAv2Data/", site, "/", UploadDate, "/queries", sep = ""))

## import all rda files 
rda_files = list.files(pattern="*.rda")
walk(rda_files, ~ load(.x, .GlobalEnv))

## bind forms 
#report <- rbindlist(mget(ls(pattern = "*query")))

report <- rbindlist(mget(ls(pattern = "*query")), fill=TRUE)
 
# st=format(Sys.time(), "%Y-%m-%d")
# write.xlsx(query_out, file = paste(site_label, "_query_report_",st,"_draft", ".xlsx", sep = ""))

#*****************************************************************************
#* Making summary tables
#*****************************************************************************
## add in new column that had form_varname_edittype
report = add_column(report, VarFormEdit=paste(report$Form, "_", report$`Variable Name`,"_", report$EditType, sep = ""))


## add in new columns for RemoveEdit variable and notes  
report = add_column(report, RemoveEdit = "", Notes = "") 


## STOP HERE TO CHECK FOR HIGH FREQUENCY VARIABLES 

## here we can remove any variables that pulled a lot of queries 
# we can note them but then remove from the query report tab (still include them in the last Form_Varname_Edit tab)
# Examples here might include a specific lab that is pulling 150 queries - this might be a range or unit issue - just bring up in email

high_freq_to_exclude <- report %>% 
  group_by(VarFormEdit) %>% 
  count(name ="Frequency") %>% 
  filter(Frequency > 50) %>%     ## let's say we want to remove all instances that happened >50 times 
  pull(VarFormEdit) ## get variable names to exclude 

high_freq_to_exclude_outrange <- report %>% filter(EditType == "Out of Range" | EditType == "Invalid Response Option" ) %>%
  group_by(VarFormEdit) %>%
  count(name = "Frequency") %>%
  filter(Frequency > 50) %>%  ## Remove instances that occurred more than 50 times
  pull(VarFormEdit)  ## Get variable names to exclude

## remove these high frequency variables from the report 
report_query <- report %>% filter(!(VarFormEdit %in% high_freq_to_exclude))
## get frequency table that shows the number of queries for each form-varname-edit type 
## main function is to look at high frequency variables 
table_freq_FmNmEd <- report_query %>% 
  group_by(Form_Edit_Type) %>% 
  count(name ="Frequency") %>% 
  rename("Form and Edit Type" = "Form_Edit_Type") %>% 
  filter(Frequency < 50)

report_high_freq <- report %>% filter(!(VarFormEdit %in% high_freq_to_exclude_outrange)) %>% 
  filter((VarFormEdit %in% high_freq_to_exclude))#exclude out of range variables, since the high frequency tab was created

## get frequency table that shows the number of queries for each form-edit type in the format of the high freq table
table_high_freq <- report_high_freq %>% 
  mutate(
    EditTypeID = case_when(
      EditType == "Duplicate ID" ~ "01", 
      EditType == "Extra Variable" ~ "02", 
      EditType == "Missing Variable" ~ "03", 
      EditType == "MomID Missing Enrollment Form" ~ "04", 
      EditType == "Visit Type Error" ~ "06",
      EditType == "InfantID Missing Delivery Form" ~ "07", 
      EditType == "MomID Ineligible" ~ "08",
      EditType == "Invalid Response Option" ~ "09"
    ),
    Recommendations = case_when(
      EditTypeID %in% c("02", "03") ~ "Consult the Data Dictionary to Harmonize Variable Name",
      EditTypeID %in% c("01") ~ "Remove all duplicate data points in dataset",
      EditTypeID %in% c("04") ~ "Confirm that all MOM have an enrollment form",
      EditTypeID %in% c("06") ~ "Correct all discrepancies in Visit_Type",
      EditTypeID %in% c("07") ~ "Include missing forms for infants",
      EditTypeID %in% c("08") ~ "Confirm that all MOMs are eligible for study",
      EditTypeID %in% c("09") ~ "Consult the Data Dictionary and confirm response options"
      
    )
  ) %>% 
  select(-EditTypeID) %>% 
  group_by(EditType, Form_Edit_Type, Recommendations) %>% 
  count(name = "Frequency") %>% 
  mutate (form = "NA", response = "*", min = "*", max = "*",DefaultValue = "*", response_range = "*") %>% 
  rename("Form and Edit Type" = "Form_Edit_Type")

#For Out_of_Range

report_query_outrange <- report %>% filter((VarFormEdit %in% high_freq_to_exclude)) %>%  
  filter((VarFormEdit %in% high_freq_to_exclude_outrange)) 

table_high_freq_outrange <- report_query_outrange %>% 
  rename("response" = "Variable Value")%>%  rename("varname" = "Variable Name")%>% 
  mutate(
    EditTypeID = case_when(
      EditType == "Out of Range" ~ "01"
    ),
    Recommendations = case_when(
      EditTypeID %in% c("01") ~ "Provide valid date/data in the dataset",
    )
  ) %>% 
  select(-EditTypeID) %>% 
  group_by(VarFormEdit, response, Recommendations, Form, varname, EditType) %>% 
  count(name = "count") %>% 
  mutate (min = "*", max = "*",DefaultValue = "*", response_range = "*") %>% 
  rename("Form and Edit Type" = "VarFormEdit") %>% 
  rename("form" = "Form")

combined_outrange <- bind_rows(
  High_Freq_Invalid_Query,
  table_high_freq_outrange %>% anti_join(High_Freq_Invalid_Query, by = "varname")
)  %>% filter(count > 40)  %>% 
  rename("Frequency" = "count")


table_high_freq_new <- bind_rows(combined_outrange, table_high_freq) %>% select ('Form and Edit Type', varname, form, EditType, response, Frequency, min, max, response_range, DefaultValue, Recommendations)


#*****************************************************************************
#* Extracting previous week's queries 
#*****************************************************************************
# we want to remove any query that the site said was not actually something that needed to be flagged - remove below
# Step 1. Import previous query report (this should be the report with sites comments)
# query report tab
previous_report_site_response <- read_excel(paste("~/PRiSMAv2Data/", site, "/", PrevUploadDate, "/",site_label,
                                                  "_query_report_", PrevUploadDate, "_response.xlsx", sep = "" ), sheet = "Query Report")
## high frequency tab 
previous_report_site_response_highfreq <- read_excel(paste("~/PRiSMAv2Data/", site, "/", PrevUploadDate, "/",site_label,
                                                           "_query_report_", PrevUploadDate, "_response.xlsx", sep = "" ), sheet = "High Frequency Tab")
 
## Step 2a. Extract the Query IDs that are not queries (Remove edit == 1)
queryid_to_remove = previous_report_site_response %>%
  #select(QueryID, `Remove edit (if true query, put 0, it not please put 1)`) %>%  ## select the columns you need
  rename("RemoveEdit" = `Remove edit (if true query, put 0, it not please put 1)`) %>% ## rename remove edit column
  filter(RemoveEdit == 1) %>% ## only keep the queries that should be removed
  relocate(QueryID, .before = ScrnID) %>% 
  select(-Notes, -UploadDate, - DateEditReported, -VisitType)

## Step 2b. call in non-query running document 
non_queries <- read_excel(paste("~/PRiSMAv2Data/", site, "/", site_label, "_non-queries-ongoing", ".xlsx", sep = "" ))

## Step 2c. rbind all new non-queries into running document
non_queries_merged <- bind_rows(non_queries, queryid_to_remove)
non_queries_merged <- non_queries_merged %>% distinct() ## extract unique elements (only want the newest entries included)

## Step 2d. Extract all non-queries into running document 
write.xlsx(non_queries_merged, file = paste("~/PRiSMAv2Data/", site, "/", site_label, "_non-queries-ongoing", ".xlsx", sep = ""))

## Step 2e. Convert to vector
queryid_to_remove_vec =  queryid_to_remove %>% pull(QueryID) ## extract vector of query IDs to remove

## Step 3. Remove the query ids that should not be pulled from THIS week's report 
report_query_new = report_query %>% filter(!(QueryID %in% queryid_to_remove))

## export this week's query report all to a new excel sheet 
st=format(Sys.time(), "%Y-%m-%d")
xl_lst <- list('Summary' = table_freq_FmNmEd, 'Query Report' = report_query, 'High Frequency Tab' = table_high_freq_new)
write.xlsx(xl_lst, file = paste(site_label, "_query_report_", st, ".xlsx", sep = ""))

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
# write.xlsx(xl_lst, file = paste(site_label, "_query_report_",st, ".xlsx", sep = ""))

