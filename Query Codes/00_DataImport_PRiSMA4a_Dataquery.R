#*****************************************************************************
#*QUERY #00 -- Import all raw .csv files from each upload 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 13 March 2023

#*Input: Raw Data 
#*Function: import raw data from sites upload to synapse 
#*Output: Two .RData files of the data - wide and long 
#*****************************************************************************

#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* 1. Change working directory to a site-specific folder OFF network drive

#* Once the previous lines of code are updated, you can highlight the entire script and run 
#*****************************************************************************

# clear environment 
rm(list = ls())

## load packages 
library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(data.table)

## 1. UPDATE EACH RUN ## 
## 1. Change working directory to a site-specific folder 
setwd("~/PRiSMAv2Data/Kenya/2023-02-10/data")

## make a list of all the .csv files in the data upload 
dfs<-Filter(function(x) is.data.frame(get(x)) , ls())

## save as a .RData file in wide format 
save(list=dfs, file= paste(UploadDate,"_", "wide",".RData",sep = ""))

## convert to long format 
setcolfirst = function(DT, ...){
  nm = as.character(substitute(c(...)))[-1L]
  setcolorder(DT, c(nm, setdiff(names(DT), nm)))
}

if (exists("mnh00")==TRUE){
    mnh00_long <- mnh00 %>% bind_cols(INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH00") %>% 
      gather(varname, response, SCRN_OBSLOC:FORMCOMPLID_MNH00, factor_key=TRUE) %>% rename("DateFormCompleted" = "SCRN_OBSSTDAT")
  
  }
if (exists("mnh01")==TRUE){
    mnh01_long <- mnh01 %>% bind_cols(INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH01") %>% 
      gather(varname, response, US_OHOLOC:FORMCOMPLID_MNH01, factor_key=TRUE) %>% rename("DateFormCompleted" = "US_OHOSTDAT")
  
  }
if (exists("mnh02")==TRUE){
     mnh02_long <- mnh02 %>%  bind_cols(INFANTID=NA)  %>% mutate(form = "MNH02") %>% rename("DateFormCompleted" = "SCRN_OBSSTDAT") %>%  setcolfirst(SCRNID, MOMID, PREGID, INFANTID, DateFormCompleted)  %>% 
       gather(varname, response, SCRN_OBSLOC:FORMCOMPLID_MNH02, factor_key=TRUE) %>% select(SCRNID, MOMID, PREGID,INFANTID, DateFormCompleted, form, varname, response)
     
  }
if (exists("mnh03")==TRUE){
     mnh03_long <- mnh03 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH03") %>% 
         gather(varname, response, SD_OBSLOC:FORMCOMPLID_MNH03, factor_key=TRUE) %>% rename("DateFormCompleted" = "SD_OBSSTDAT")
     
} 
if (exists("mnh04")==TRUE){
       mnh04_long <- mnh04 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH04") %>% 
         gather(varname, response, ANC_OBSLOC:FORMCOMPLID_MNH04, factor_key=TRUE) %>% rename("DateFormCompleted" = "ANC_OBSSTDAT")
       
  }
if (exists("mnh05")==TRUE){
    mnh05_long <- mnh05 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH05") %>% 
      gather(varname, response, ANT_OBSLOC:FORMCOMPLID_MNH05, factor_key=TRUE) %>% rename("DateFormCompleted" = "ANT_PEDAT") 
    
  }
if (exists("mnh06")==TRUE){
    mnh06_long <- mnh06 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID)%>% mutate(form = "MNH06") %>% 
      gather(varname, response, DIAG_OBSLOC:FORMCOMPLID_MNH06, factor_key=TRUE) %>% rename("DateFormCompleted" = "DIAG_VSDAT" ) 
    
  }
if (exists("mnh07")==TRUE){
      mnh07_long <- mnh07 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH07") %>% 
        gather(varname, response, MAT_SPEC_COLLECT_LOC:FORMCOMPLID_MNH07, factor_key=TRUE) %>% rename("DateFormCompleted" = "MAT_SPEC_COLLECT_DAT" )
      
  }
if (exists("mnh08")==TRUE){
      mnh08_long <- mnh08 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH08") %>% 
        gather(varname, response, VISIT_LBSTDAT:FORMCOMPLID_MNH08, factor_key=TRUE) %>% rename("DateFormCompleted" = "LBSTDAT" ) 
      
  }
if (exists("mnh09")==TRUE){
    mnh09_long <- mnh09 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH09") %>% 
      gather(varname, response, MAT_LD_OHOSTTIM:FORMCOMPLID_MNH09, factor_key=TRUE) %>% rename("DateFormCompleted" = "MAT_LD_OHOSTDAT" ) 
      
  }
if (exists("mnh10")==TRUE){
    mnh10_long <- mnh10 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID, FORMCOMPLDAT_MNH10) %>% mutate(form = "MNH10") %>% 
      gather(varname, response, MAT_VITAL_MNH10:FORMCOMPLID_MNH10, factor_key=TRUE) %>% rename("DateFormCompleted" = "FORMCOMPLDAT_MNH10") 

  }
if (exists("mnh11")==TRUE){
    mnh11_long <- mnh11 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH11") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH11, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISIT_OBSSTDAT") 
      
  }
if (exists("mnh12")==TRUE){
    mnh12_long <- mnh12 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH12") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH12, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISIT_OBSSTDAT") 
      
  }
if(exists("mnh13")==TRUE){
    mnh13_long <- mnh13 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH13") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH13, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISIT_OBSSTDAT") 

  }
if (exists("mnh14")==TRUE){
    mnh14_long <- MNH14 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VISIT_OBSSTDAT) %>% mutate(form = "MNH14") %>% 
      gather(varname, response, BIRTH_ORDER_RPORRES:FORMCOMPLID_MNH14, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISIT_OBSSTDAT") 
    
  }
if (exists("mnh15")==TRUE){
    mnh15_long <- mnh15 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH15") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH15, factor_key=TRUE) %>% rename("DateFormCompleted" = "OBSSTDAT") 
  
  }
if (exists("mnh16")==TRUE){
    mnh16_long <- mnh16 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH16") %>% 
      gather(varname, response, MAT_VITAL_MNH16:FORMCOMPLID_MNH16, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISDAT") 
  
  }
if (exists("mnh17")==TRUE){
    mnh17_long <- mnh17 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH17") %>% 
      gather(varname, response, MAT_VITAL_MNH17:FORMCOMPLID_MNH17, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISDAT") 
  
  }
if (exists("mnh18")==TRUE){
    mnh18_long <- mnh18 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH18") %>% 
      gather(varname, response, MAT_VITAL_MNH18:FORMCOMPLID_MNH18, factor_key=TRUE) %>% rename("DateFormCompleted" = "VISDAT") 
    
  }
if (exists("mnh19")==TRUE){
    mnh19_long <- mnh19 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH19") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH19, factor_key=TRUE) %>% rename("DateFormCompleted" = "OBSSTDAT") 
    
  }
if (exists("mnh20")==TRUE){
    mnh20_long <- mnh20 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH20") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH20, factor_key=TRUE) %>% rename("DateFormCompleted" = "OBSSTDAT") 
    
  }
if (exists("mnh21")==TRUE){
    mnh21_long <- mnh21 %>% bind_cols(SCRNID = NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH21") %>% 
      gather(varname, response, AESTTIM:FORMCOMPLDAT_MNH21, factor_key=TRUE) %>% rename("DateFormCompleted" = "AESTDAT") 
    
  }
if (exists("mnh25")==TRUE){
    mnh25_long <- mnh25 %>% bind_cols(SCRNID = NA, INFANTID=NA) %>% setcolfirst(SCRNID, MOMID, PREGID, INFANTID) %>% mutate(form = "MNH25") %>% 
      gather(varname, response, VISIT_OBSLOC:FORMCOMPLID_MNH25, factor_key=TRUE) %>% rename("DateFormCompleted" = "OBSSTDAT") 
  }
  
long_list = mget(ls(pattern = "*_long"))                              # make list of all of long forms 
long_list_names = as.vector(names(long_list))                         # get form names that are included in the long format 
form_num <- as.vector(str_extract(long_list_names, '.*(?=\\_long)'))  # clean up names for the long format 
data_long = rbindlist(mget(ls(pattern = "*_long")))                   # bind all the long forms together 

## save data frames as RData file to import into other scripts 
setwd("~/PRiSMAv2Data/Kenya/2023-02-10/data")
save(list=c("data_long", "form_num"), file= paste(UploadDate,"_","long",".RData",sep = ""))


