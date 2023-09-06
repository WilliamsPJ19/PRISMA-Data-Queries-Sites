Skip to content
PRiSMA-Study
/
PRISMA-Data-Queries-Sites

Type / to search

Code
Issues
Pull requests
Actions
Projects
Wiki
Security
Insights
Settings
BreadcrumbsPRISMA-Data-Queries-Sites/R Query Codes
/
03_OutRange_PRiSMA4a_Dataquery.R
in
main

Edit

Preview
Indent mode

Spaces
Indent size

2
Line wrap mode

No wrap
Editing 03_OutRange_PRiSMA4a_Dataquery.R file contents
Selection deleted
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
73
74
75
76
77
#*****************************************************************************
#*QUERY #3 -- CHECK FOR OUT OF RANGE VALUES 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 06 September 2023

#*Input: Long data 
#*Function: Extract any values that either (1) do not match a valid response options or (2) is out of range 
#*Output: .rda file with all out of range values 

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

# UPDATE EACH RUN: load in the LONG data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_long.Rdata", sep = "")) 

## UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

#* import data dictionary
varNames_sheet <- read_excel("~/PRiSMAv2Data/Queries/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
varNames_sheet <- varNames_sheet %>% dplyr::select(Form, `Variable Name`, `Response Options`,
                                                   `Query Category`,`Value`, `Field Type (Date, Time, Number, Text)`,
                                                   `Minimum Value`, `Maximum Value`) %>% 
                                     mutate(`Variable Name` = toupper(varNames_sheet$`Variable Name`)) %>% 
                                     rename("varname" = `Variable Name`, "form" = "Form", "ResponseRange" = `Value`)


# import excel sheet with ranges for fetal biometry ranges. Valid ranges for these variables depend on gestational age -the excel sheet has the valid ranges by GA
fetalRange_sheet <- read_excel("~/PRiSMAv2Data/Queries/fetal_biometry_range.xlsx")

## create function to set column order 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}


#*****************************************************************************
#* Fetal Biometry Checks 
#******************************************************************************
#* FL_PERES_01_FTS1; FL_PERES_MEAN_FTS1 
#* AC_PERES_01_FTS1; AC_PERES_MEAN_FTS1
#* HC_PERES_01_FTS1; HC_PERES_MEAN_FTS1
#* BPD_PERES_01_FTS1; BPD_PERES_MEAN_FTS1

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

Use Control + Shift + m to toggle the tab key moving focus. Alternatively, use esc then tab to move to the next interactive element on the page.
Editing PRISMA-Data-Queries-Sites/R Query Codes/03_OutRange_PRiSMA4a_Dataquery.R at main · PRiSMA-Study/PRISMA-Data-Queries-Sites
