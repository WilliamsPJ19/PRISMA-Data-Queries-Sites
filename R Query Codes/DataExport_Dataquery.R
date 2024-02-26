#*****************************************************************************
#*QUERY #6 -- Generate Query Report 
#* Written by: Stacie Loisate & Xiaoyan Hu & Precious Williams
#* Last updated: 26 February 2024

#*Input: all .rds files 
#*Function: merge all queries together and assign query ID 
#*Output: excel sheet with a full query report 
#*****************************************************************************
#*****************************************************************************
#* Data setup
#*****************************************************************************

# clear environment 
#rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: Set "site" variable to the site you are running the query for 
site = "Kenya"

# UPDATE EACH RUN: load in the PREVIOUS WEEK's query that has the "RemoveEdit" column filled out
previous_report_site_response <- read_excel(paste0("~/PRiSMAv2Data/Kenya/2023-08-11/PRISMA-Kenya_query_report_2023-08-11_response.xlsx", sep = ""),
                                            sheet = "Query Report") 

# UPDATE EACH RUN: load in on-going "non-queries" document 
non_queries <-  read_excel(paste0("~/PRiSMAv2Data/Kenya/PRISMA-Kenya_non-queries-ongoing.xlsx", sep = ""))

# UPDATE EACH RUN: set path to location where query output is stored
path_to_data <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

# UPDATE EACH RUN: set path to location where you want to save the final query report output 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

## pull all .csv files from the folder specified above 
query_list <- list() # create an empty list first.
list_query <- dir(path = path_to_data, pattern = "*.rda", full.names = TRUE) #creates a list of all the csv files in the directory
for (data_file in list_query[]) { #can test by just bringing in a small number (add 1:2 inside the bracket to do so)
  form_num <- basename(data_file) #basename() pulls out just the name of the file from the entire directory/path.
  print(paste("Reading", form_num))
  query_list[[form_num]] <- load(data_file)
}

#High_Freq_Invalid_query = High_Freq_Invalid_extra_tab
## Merge all the queries togehter and convert all varables to character 
  # converting to character will avoid any merging issues between columns of different classes
report <- rbindlist(lapply(mget(ls(pattern = "*query$")), function(x) { 
  x[] <- lapply(x,type.convert, as.is = TRUE); x}),fill=TRUE)



#*****************************************************************************
#* Generating summary tables -- High Frequency Tab
# here we can remove any variables that pulled a lot of queries 
# we can note them but then remove from the query report tab (still include them in the last Form_Varname_Edit tab)
# Examples here might include a specific lab that is pulling 150 queries - this might be a range or unit issue - just bring up in email


#* Note: We will separate out the non-out of range queries and the out of range queries
## The out of range queries have an added step where the min and max from the data dictionary is merged in 
#*****************************************************************************
## add in new column that had form_varname_edittype
report = add_column(report, VarFormEdit=paste(report$Form, "_", report$`Variable Name`,"_", report$EditType, sep = ""))

## add in new columns for RemoveEdit variable and notes  
report = add_column(report, RemoveEdit = "", Notes = "") 

# extract non-out of range queries that are pulled >100 times in the query report 
high_freq_to_exclude <- report %>% 
  group_by(VarFormEdit) %>% 
  count(name ="Frequency") %>% 
  filter(Frequency > 100) %>%     ## let's say we want to remove all instances that happened >100 times 
  pull(VarFormEdit) ## get variable names to exclude 

# extract out of range queries that are pulled >100 times in the query report 
high_freq_to_exclude_outrange <- report %>% filter(EditType == "Out of Range" | EditType == "Invalid Response Option" ) %>%
  group_by(VarFormEdit) %>%
  count(name = "Frequency") %>%
  filter(Frequency > 100) %>%  ## Remove instances that occurred more than 100 times
  pull(VarFormEdit)  ## Get variable names to exclude

## remove these high frequency variables from the report 
report_query <- report %>% filter(!(VarFormEdit %in% high_freq_to_exclude))

table_freq_FmNmEd <- report_query %>% 
  group_by(Form_Edit_Type) %>% 
  count(name ="Frequency") %>% 
  rename("Form and Edit Type" = "Form_Edit_Type") %>% 
  filter(Frequency < 100)

## filter out high frequency queries from the report 
report_high_freq <- report %>% filter(!(VarFormEdit %in% high_freq_to_exclude_outrange)) %>% 
  filter((VarFormEdit %in% high_freq_to_exclude)) #exclude out of range variables, since the high frequency tab was created

######## NON- OUT OF RANGE QUERIES ######## 
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
      EditType == "Invalid Response Option" ~ "09",
      startsWith (EditType, "Missing") ~ "10",
      EditType == "Missing US EDD at Enrolment" ~ "11",
      str_detect(EditType, "EDD") ~ "12",
      startsWith (EditType, "Visit Type") ~  "13"
    ),
    Recommendations = case_when(
      EditTypeID %in% c("02", "03") ~ "Consult the Data Dictionary to Harmonize Variable Name",
      EditTypeID %in% c("01") ~ "Remove all duplicate data points in dataset",
      EditTypeID %in% c("04") ~ "Confirm that all MOM have an enrollment form",
      EditTypeID %in% c("06") ~ "Correct all discrepancies in Visit_Type",
      EditTypeID %in% c("07") ~ "Include missing forms for infants",
      EditTypeID %in% c("08") ~ "Confirm that all MOMs are eligible for study",
      EditTypeID %in% c("09") ~ "Consult the Data Dictionary and confirm response options",
      EditTypeID %in% c("10") ~ "Provide the specific missing form for the specified visit",
      EditTypeID %in% c("12") ~ "Confirm the that the Estimated Due Date has accurate data",
      EditTypeID %in% c("11") ~ "Ensure that Estimated Due Date at Ultrasound is Not Missing",
      EditTypeID %in% c("13") ~ "Check the Invalid Visit Type Tab for specific data error"
    )
  ) %>% 
  select(-EditTypeID) %>% 
  group_by(EditType, Form_Edit_Type, Recommendations) %>% 
  count(name = "Frequency") %>% 
  mutate (form = "NA", response = "*", min = "*", max = "*",DefaultValue = "*", response_range = "*") %>% 
  rename("Form and Edit Type" = "Form_Edit_Type")

######## OUT OF RANGE QUERIES ######## 
# extract all out of range and invalid response option queries that are pulled >100 times in the query report 
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

High_Freq_Invalid_Query$response = as.character(High_Freq_Invalid_Query$response)
table_high_freq_outrange$response = as.character(table_high_freq_outrange$response)

combined_outrange <- bind_rows(
  High_Freq_Invalid_Query,
  table_high_freq_outrange %>% anti_join(High_Freq_Invalid_Query, by = "varname")
)  %>% 
  #filter(count > 10)  %>% 
  group_by(varname, form) %>% 
  mutate(response = paste0(min(as.numeric(response)), "-", max(as.numeric(response)))) %>%
  mutate(count = n()) %>% 
  dplyr::rename("Frequency" = "count") %>% 
  distinct()


table_high_freq_to_export <- bind_rows(combined_outrange, table_high_freq) %>% 
  select ('Form and Edit Type', varname, form, EditType, response, Frequency, min, max, response_range, DefaultValue, Recommendations)

#*****************************************************************************
#* Extracting previous week's queries 
#*****************************************************************************
# we want to remove any query that the site said was not actually something that needed to be flagged - remove below
## Step 1. Extract the Query IDs that are not queries (Remove edit == 1)
queryid_to_remove = previous_report_site_response %>%
  #select(QueryID, `Remove edit (if true query, put 0, it not please put 1)`) %>%  ## select the columns you need
  #rename("RemoveEdit" = `Remove edit (if true query, put 0, it not please put 1)`) %>% ## rename remove edit column
  filter(RemoveEdit == 1) %>% ## only keep the queries that should be removed
  relocate(QueryID, .before = ScrnID) %>% 
  select(-Notes, -UploadDate, -VisitType, -VarFormEdit, -DateEditReported, -RemoveEdit)

## Step 2. rbind all new non-queries into running document
non_queries_merged <- bind_rows(non_queries, queryid_to_remove)

non_queries_merged <- non_queries_merged %>% distinct() ## extract unique elements (only want the newest entries included)

## Step 3. Extract all non-queries into running document 
write.xlsx(non_queries_merged, file = paste("~/PRiSMAv2Data/", site, "/", site_label, "_non-queries-ongoing", ".xlsx", sep = ""))

## Step 4. Convert to vector
queryid_to_remove_vec =  non_queries_merged %>% pull(as.vector(QueryID)) ## extract vector of query IDs to remove

## Step 3. Remove the query ids that should not be pulled from THIS week's report 
report_query_to_export = report_query %>% filter(!(QueryID %in% queryid_to_remove_vec))%>% 
  filter(!((VisitType %in% c(13,14))))

#*****************************************************************************
#* Removing "non-queries" from additional tabs in the report
#*****************************************************************************

if (exists("EDD_query_comments_to_export")== TRUE){
  EDD_query_comments_to_export = EDD_query_comments %>% filter(!(QueryID %in% queryid_to_remove_vec))
}

if (exists("visit_type_query_extra_tab_to_export")== TRUE){
  visit_type_query_extra_tab_to_export = visit_type_query_extra_tab %>% filter(!(QueryID %in% queryid_to_remove_vec))
}
#*****************************************************************************
#* Generating summary table 
#*****************************************************************************

table_freq_FmNmEd <- report_query_to_export %>%
  filter(!(`Variable Name` %in% high_freq_to_exclude)) %>%  ## remove high frequency variables 
  group_by(Form_Edit_Type) %>%
  count(name ="Frequency")  %>%
  rename("Form and Edit Type" = "Form_Edit_Type") %>%
  filter(!(is.na(`Form and Edit Type`)))

#*****************************************************************************
#* export this week's query report all to a new excel sheet 
#*****************************************************************************
st=format(Sys.time(), "%Y-%m-%d")
xl_lst <- list('Summary' = table_freq_FmNmEd, 'Query Report' = report_query_to_export,
               'High Frequency Tab' = table_high_freq_to_export,
               'Invalid Visit Types' = visit_type_query_extra_tab_to_export,
               'Invalid EDDs' = EDD_query_comments_to_export
)

write.xlsx(xl_lst, file = paste0(path_to_data,"/",  site_label, "_query_report_", st, ".xlsx"))

