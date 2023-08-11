#*****************************************************************************
#*QUERY #00 -- Import all raw .csv files from each upload 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 06 September 2023


#*Input: Raw Data 
#*Function: import raw data from sites upload to synapse 
#*Output: Two .RData files of the data - wide and long 

#* Notes:
#* load in .csvs and confirm the following: 
# 1. Each .csv is saved with the lowercase form number with ".csv" - Example. "mnh00.csv"
# 2. column names within each dataframe should be capitalized 
#*****************************************************************************
#*****************************************************************************
## Data setup ## 
#*****************************************************************************

## load packages 
library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(data.table)


  ## UPDATE EACH RUN: set upload date - this will help with saving each of the files 
  UploadDate = "2023-08-25"
  
  ## UPDATE EACH RUN: set path to location where data is stored
  path_to_data <- "Z:/SynapseCSVs/Kenya/2023-08-25/"
  
  ## UPDATE EACH RUN: set path to location where you want to save the output below 
  path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/data/"

  ## pull all .csv files from the folder specified above 
  mnh_list <- list() # create an empty list first.
  list_mnh <- dir(path = path_to_data, pattern = "*csv", full.names = TRUE) #creates a list of all the csv files in the directory
  for (data_file in list_mnh[]) { #can test by just bringing in a small number (add 1:2 inside the bracket to do so)
    form_num <- substr(basename(data_file), 1,5) #substr pulls out the 1:5 spaces in a char (will pull out “mnh00" etc);
    #basename() pulls out just the name of the file from the entire directory/path.
    print(paste("Reading", form_num))
    mnh_list[[form_num]] <- read.csv(data_file)
  }

  #  make sure all column names are uppercase 
  mnh_list <- lapply(mnh_list, function (x){
    upper <- toupper(names(x))
    setnames(x, upper)
  })
  
  # extract each list item as its own dataframe in the environment
  list2env(mnh_list, globalenv())
  
#*****************************************************************************
# Convert data to long format
  # Some queries require the data to be in long format  
#*****************************************************************************
  
## create function to set column order 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}


if (exists("mnh00")==TRUE){
  
  mnh00_long <- mnh00 %>% bind_cols(INFANTID=NA) %>% ## we want to have the option to bind_rows of the long data, but some forms don'thave infantid -- add a null column here 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, SCRN_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, SCRN_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "SCRN_OBSSTDAT") %>%  mutate(form = "MNH00")
  
}

if (exists("mnh01")==TRUE){
  mnh01_long <- mnh01 %>% bind_cols(INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, US_OHOSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "US_OHOSTDAT") %>%  mutate(form = "MNH01")

}

if (exists("mnh02")==TRUE){
  
  mnh02_long <- mnh02 %>% bind_cols(INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, SCRN_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, SCRN_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "SCRN_OBSSTDAT") %>%  mutate(form = "MNH02")

}

if (exists("mnh03")==TRUE){

  mnh03_long <- mnh03 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, SD_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, SD_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "SD_OBSSTDAT") %>%  mutate(form = "MNH03")
  
}

if (exists("mnh04")==TRUE){
  
  mnh04_long <- mnh04 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, ANC_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, ANC_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "ANC_OBSSTDAT") %>%  mutate(form = "MNH04")
  
}


if (exists("mnh05")==TRUE){
  
  mnh05_long <- mnh05 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, ANT_PEDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, ANT_PEDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "ANT_PEDAT") %>%  mutate(form = "MNH05")
}


if (exists("mnh06")==TRUE){
  
  mnh06_long <- mnh06 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, DIAG_VSDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, DIAG_VSDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "DIAG_VSDAT") %>%  mutate(form = "MNH06")

}

if (exists("mnh07")==TRUE){
  
  mnh07_long <- mnh07 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, MAT_SPEC_COLLECT_LOC) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, MAT_SPEC_COLLECT_LOC), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "MAT_SPEC_COLLECT_LOC") %>%  mutate(form = "MNH07")
  
}


if (exists("mnh08")==TRUE){
  
  mnh08_long <- mnh08 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, LBSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, LBSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "LBSTDAT") %>%  mutate(form = "MNH08")
  
}

if (exists("mnh09")==TRUE){

  ## first need to make m09 long format for each infant 
  m09_INF1 <- mnh09 %>% 
    rename("INFANTID" = "INFANTID_INF1") %>% 
    filter(INFANTID != "n/a")
  
  m09_INF2 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF2") %>% 
    filter(INFANTID != "n/a")
  
  m09_INF3 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF3") %>% 
    filter(INFANTID != "n/a")
  
  m09_INF4 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF4") %>%
    filter(INFANTID != "n/a")
  
  ## bind all infants together 
  mnh09_update <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
  
  ## remove INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4
  infantids_to_remove <- c("INFANTID_INF1", "INFANTID_INF2", "INFANTID_INF3", "INFANTID_INF4")
  mnh09_update <- mnh09_update %>% select(-any_of(infantids_to_remove))

  mnh09_long <- mnh09_update %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, MAT_LD_OHOSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, MAT_LD_OHOSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "MAT_LD_OHOSTDAT") %>%  mutate(form = "MNH09")
  
}

if (exists("mnh10")==TRUE){
  
  mnh10_long <- mnh10 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISIT_OBSSTDAT") %>%  mutate(form = "MNH10")

}

if (exists("mnh11")==TRUE){
  
  mnh11_long <- mnh11 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISIT_OBSSTDAT") %>%  mutate(form = "MNH11")
  
}

if (exists("mnh12")==TRUE){
  
  mnh12_long <- mnh12 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISIT_OBSSTDAT") %>%  mutate(form = "MNH12")
  
}
if(exists("mnh13")==TRUE){
  
  mnh13_long <- mnh13 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISIT_OBSSTDAT") %>%  mutate(form = "MNH13")
  
}

if (exists("mnh14")==TRUE){
  
  mnh14_long <- mnh14 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISIT_OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISIT_OBSSTDAT") %>%  mutate(form = "MNH14")
  
  
}

if (exists("mnh15")==TRUE){
  
  mnh15_long <- mnh15 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "OBSSTDAT") %>%  mutate(form = "MNH15")
  
}

if (exists("mnh16")==TRUE){
  
  mnh16_long <- mnh16 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISDAT") %>%  mutate(form = "MNH16")
  
}

if (exists("mnh17")==TRUE){
  
  mnh17_long <- mnh17 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISDAT") %>%  mutate(form = "MNH17")
  

}

if (exists("mnh18")==TRUE){
  
  mnh18_long <- mnh18 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, VISDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, VISDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "VISDAT") %>%  mutate(form = "MNH18")
  
}
if (exists("mnh19")==TRUE){
  
  mnh19_long <- mnh19 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "OBSSTDAT") %>%  mutate(form = "MNH19")
  
}
if (exists("mnh20")==TRUE){
  
  mnh20_long <- mnh20 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "OBSSTDAT") %>%  mutate(form = "MNH20")
  
}

if (exists("mnh21")==TRUE){
  
  mnh21_long <- mnh21 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, AESTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, AESTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "AESTDAT") %>%  mutate(form = "MNH21")
  

}

if (exists("mnh22")==TRUE){
  
  mnh22_long <- mnh22 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, DVSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, DVSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "DVSTDAT") %>%  mutate(form = "MNH22")
  
}

if (exists("mnh23")==TRUE){
  
  mnh23_long <- mnh23 %>% bind_cols(SCRNID = NA, INFANTID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, CLOSE_DSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, CLOSE_DSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "CLOSE_DSSTDAT") %>%  mutate(form = "MNH23")
  
}


if (exists("mnh24")==TRUE){
  
  mnh24_long <- mnh24 %>% bind_cols(SCRNID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, CLOSE_DSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, CLOSE_DSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "CLOSE_DSSTDAT") %>%  mutate(form = "MNH24")
  
}

if (exists("mnh25")==TRUE){
  
  mnh25_long <- mnh25 %>% bind_cols(SCRNID = NA, INFANTID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, OBSSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "OBSSTDAT") %>%  mutate(form = "MNH25")

}

if (exists("mnh26")==TRUE){
  
  mnh26_long <- mnh26 %>% bind_cols(SCRNID = NA, INFANTID = NA) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID, FTGE_OBSTDAT) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = -c(SCRNID, MOMID, PREGID, INFANTID, FTGE_OBSTDAT), 
                 names_to = "varname", values_to = "response") %>% 
    rename("VisitDate" = "FTGE_OBSTDAT") %>%  mutate(form = "MNH26")
  
}

long_list = mget(ls(pattern = "*_long"))                              # make list of all of long forms 
long_list_names = as.vector(names(long_list))                         # get form names that are included in the long format 
form_num <- as.vector(str_extract(long_list_names, '.*(?=\\_long)'))  # clean up names for the long format 
data_long = rbindlist(mget(ls(pattern = "*_long")))                   # bind all the long forms together 


#*****************************************************************************
# Save data frames as RData file to import into other scripts 
#*****************************************************************************
## export wide data into new folder
dfs<-Filter(function(x) is.data.frame(get(x)) , ls())
save(list=dfs, file= paste0(path_to_save, UploadDate, "_wide",".RData",sep = ""))
## export long data into new folder
save(list=c("data_long", "form_num"), file= paste0(path_to_save, UploadDate, "_long",".RData",sep = ""))
