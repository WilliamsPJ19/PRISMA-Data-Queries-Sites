#*****************************************************************************
#*QUERY #1 -- CHECK FOR CORE VARIABLE NAMES 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 20 October  2023

#*Input: Wide data (all raw .csv files)
#*Function: check to make sure all variables exist in the data and match data dictionary formatting 
#*Output: .rda file with all missing or extra variable names 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for 
#* 3. Set your main directory 
#* 
#* Once the previous lines of code are updated, you can highlight the entire script and run 

#* Notes: 
#* Make sure the data dictionary is in the correct folder 
#*****************************************************************************
#*****************************************************************************
#* DATA SETUP
#*****************************************************************************

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(stringr)

# UPDATE EACH RUN: set site variable - this is nsecessary to call in the correct MNH25 variables from the data dictionary (each site has their own MNH25)
site = "Kenya"

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

## UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

## call in the data dictionary 
variable_names <- read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-GW/R/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx")
variable_names <- variable_names %>% select(Form, `Variable Name`)
names(variable_names) = c("Form", "VarName")
variable_names$VarName = toupper(variable_names$VarName)

#* Make empty dataframe to store the extracted queries in: 
#* variables that are present in the data but not the data dictionary will be flagged as "extra variables" in VarNamesExtra
VarNamesExtra <- as.data.frame(matrix(nrow = 1, ncol = 2))
names(VarNamesExtra) = c("Form", "Extra Variables")

#* variables that are present in the data dictionary but not the data will be flagged as "missing variables" in VarNamesMissing
VarNamesMissing <- as.data.frame(matrix(nrow = 1, ncol = 2))
names(VarNamesMissing) = c("Form", "Missing Variables")

## The following code will loop through each form to identify any extra or missing variables 
#*****************************************************************************
#*MNH00
#*****************************************************************************
if (exists("mnh00")==TRUE){
  #*get variable list for MNH00
  VarNames_form <- variable_names %>% filter(Form == "MNH00") %>% select("VarName")
  VarNames_m00 = as.vector(VarNames_form$VarName)
  VarNames_data <- colnames(mnh00)
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m00 <- as.data.frame(setdiff(VarNames_m00,VarNames_data))
  names(VarNamesMissing_m00) = "Missing Variables"
  
  if (length(VarNamesMissing_m00 >1)) {
    VarNamesMissing_m00 = cbind(Form = "MNH00", VarNamesMissing_m00)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m00 <- as.data.frame(setdiff(VarNames_data,VarNames_m00))
  names(VarNamesExtra_m00) = "Extra Variables"
  
  if (length(VarNamesExtra_m00 >1)) {
    VarNamesExtra_m00 = cbind(Form = "MNH00", VarNamesExtra_m00)
  }
  
  #*bind with other forms
  if (nrow(VarNamesExtra_m00 > 1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m00)
  } 
  
  if (nrow(VarNamesMissing_m00 > 1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m00)
  }
  
}
#*****************************************************************************
#*MNH01
#*****************************************************************************
if (exists("mnh01")==TRUE){
  
  #*get variable list for MNH01
  VarNames_form <- variable_names %>% filter(Form == "MNH01") %>% select("VarName")
  VarNames_m01 = as.vector(VarNames_form$VarName)
  
  VarNames_data <- colnames(mnh01)
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m01 <- as.data.frame(setdiff(VarNames_m01,VarNames_data))
  names(VarNamesMissing_m01) = "Missing Variables"
  
  if (length(VarNamesMissing_m01 >1)) {
    VarNamesMissing_m01 = cbind(Form = "MNH01", VarNamesMissing_m01)
  }
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m01 <- as.data.frame(setdiff(VarNames_data,VarNames_m01))
  names(VarNamesExtra_m01) = "Extra Variables"
  
  if (length(VarNamesExtra_m01 >1)) {
    VarNamesExtra_m01 = cbind(Form = "MNH01", VarNamesExtra_m01)
  }
  
  
  #*bind with other forms
  if (nrow(VarNamesExtra_m01 > 1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m01)
  } 
  
  if (nrow(VarNamesMissing_m01 > 1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m01)
  } 
  
}
#*****************************************************************************
#*MNH02
#*****************************************************************************
if (exists("mnh02")==TRUE){
  
  #*get variable list for MNH02
  VarNames_form <- variable_names %>% filter(Form == "MNH02") %>% select("VarName")
  VarNames_m02 = as.vector(VarNames_form$VarName)
  
  VarNames_data <- colnames(mnh02)
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m02 <- as.data.frame(setdiff(VarNames_m02,VarNames_data))
  names(VarNamesMissing_m02) = "Missing Variables"
  
  if (length(VarNamesMissing_m02 >1)) {
    VarNamesMissing_m02 = cbind(Form = "MNH02", VarNamesMissing_m02)
  }
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m02 <- as.data.frame(setdiff(VarNames_data,VarNames_m02))
  names(VarNamesExtra_m02) = "Extra Variables"
  
  if (length(VarNamesExtra_m02 >1)) {
    VarNamesExtra_m02 = cbind(Form = "MNH02", VarNamesExtra_m02)
  }
  
  
  #*bind with other forms
  
  if (length(VarNamesMissing_m02 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m02)
  }
  
  if (length(VarNamesExtra_m02 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m02)
  }
}
#*****************************************************************************
#*MNH03
#*****************************************************************************
if (exists("mnh03")==TRUE){
  #*get variable list for MNH03 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH03") %>% select("VarName")
  VarNames_m03 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH03 from data  
  VarNames_data <- colnames(mnh03) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m03 <- as.data.frame(setdiff(VarNames_m03,VarNames_data))
  names(VarNamesMissing_m03) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m03 >1)) {
    VarNamesMissing_m03 = cbind(Form = "MNH03", VarNamesMissing_m03)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m03 <- as.data.frame(setdiff(VarNames_data,VarNames_m03))
  names(VarNamesExtra_m03) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m03 >1)) {
    VarNamesExtra_m03 = cbind(Form = "MNH03", VarNamesExtra_m03)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m03 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m03)
  }
  
  if (length(VarNamesExtra_m03 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m03)
  }
}

#*****************************************************************************
#*MNH04
#*****************************************************************************
if (exists("mnh04")==TRUE){
  
  #*get variable list for MNH04 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH04") %>% select("VarName")
  VarNames_m04 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH04 from data  
  VarNames_data <- colnames(mnh04) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m04 <- as.data.frame(setdiff(VarNames_m04,VarNames_data))
  names(VarNamesMissing_m04) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m04 >1)) {
    VarNamesMissing_m04 = cbind(Form = "MNH04", VarNamesMissing_m04)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m04 <- as.data.frame(setdiff(VarNames_data,VarNames_m04))
  names(VarNamesExtra_m04) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m04 >1)) {
    VarNamesExtra_m04 = cbind(Form = "MNH04", VarNamesExtra_m04)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m04 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m04)
  }
  
  if (length(VarNamesExtra_m04 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m04)
  }
  
}
#*****************************************************************************
#*MNH05
#*****************************************************************************
if (exists("mnh05")==TRUE){
  
  #*get variable list for MNH05 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH05") %>% select("VarName")
  VarNames_m05 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH05 from data  
  VarNames_data <- colnames(mnh05) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m05 <- as.data.frame(setdiff(VarNames_m05,VarNames_data))
  names(VarNamesMissing_m05) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m05 >1)) {
    VarNamesMissing_m05 = cbind(Form = "MNH05", VarNamesMissing_m05)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m05 <- as.data.frame(setdiff(VarNames_data,VarNames_m05))
  names(VarNamesExtra_m05) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m05 >1)) {
    VarNamesExtra_m05 = cbind(Form = "MNH05", VarNamesExtra_m05)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m05 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m05)
  }
  
  if (length(VarNamesExtra_m05 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m05)
  }
}
#*****************************************************************************
#*MNH06
#*****************************************************************************
if (exists("mnh06")==TRUE){
  
  #*get variable list for MNH06 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH06") %>% select("VarName")
  VarNames_m06 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH06 from data  
  VarNames_data <- colnames(mnh06) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m06 <- as.data.frame(setdiff(VarNames_m06,VarNames_data))
  names(VarNamesMissing_m06) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m06 >1)) {
    VarNamesMissing_m06 = cbind(Form = "MNH06", VarNamesMissing_m06)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m06 <- as.data.frame(setdiff(VarNames_data,VarNames_m06))
  names(VarNamesExtra_m06) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m06 >1)) {
    VarNamesExtra_m06 = cbind(Form = "MNH06", VarNamesExtra_m06)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m06 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m06)
  }
  
  if (length(VarNamesExtra_m06 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m06)
  }
}
#*****************************************************************************
#*MNH07
#*****************************************************************************
if (exists("mnh07")==TRUE){
  
  #*get variable list for MNH07 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH07") %>% select("VarName")
  VarNames_m07 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH07 from data  
  VarNames_data <- colnames(mnh07) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m07 <- as.data.frame(setdiff(VarNames_m07,VarNames_data))
  names(VarNamesMissing_m07) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m07 >1)) {
    VarNamesMissing_m07 = cbind(Form = "MNH07", VarNamesMissing_m07)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m07 <- as.data.frame(setdiff(VarNames_data,VarNames_m07))
  names(VarNamesExtra_m07) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m07 >1)) {
    VarNamesExtra_m07 = cbind(Form = "MNH07", VarNamesExtra_m07)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m07 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m07)
  }
  
  if (length(VarNamesExtra_m07 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m07)
  }
}
#*****************************************************************************
#*MNH08
#*****************************************************************************
if (exists("mnh08")==TRUE){
  
  #*get variable list for MNH08 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH08") %>% select("VarName")
  VarNames_m08 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH08 from data  
  VarNames_data <- colnames(mnh08) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m08 <- as.data.frame(setdiff(VarNames_m08,VarNames_data))
  names(VarNamesMissing_m08) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m08 >1)) {
    VarNamesMissing_m08 = cbind(Form = "MNH08", VarNamesMissing_m08)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m08 <- as.data.frame(setdiff(VarNames_data,VarNames_m08))
  names(VarNamesExtra_m08) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m08 >1)) {
    VarNamesExtra_m08 = cbind(Form = "MNH08", VarNamesExtra_m08)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m08 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m08)
  }
  
  if (length(VarNamesExtra_m08 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m08)
  }
}
#*****************************************************************************
#*MNH09
#*****************************************************************************
if (exists("mnh09")==TRUE){
  
  #*get variable list for MNH09 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH09") %>% select("VarName")
  VarNames_m09 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH09 from data  
  VarNames_data <- colnames(mnh09) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m09 <- as.data.frame(setdiff(VarNames_m09,VarNames_data))
  names(VarNamesMissing_m09) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m09 >1)) {
    VarNamesMissing_m09 = cbind(Form = "MNH09", VarNamesMissing_m09)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m09 <- as.data.frame(setdiff(VarNames_data,VarNames_m09))
  names(VarNamesExtra_m09) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m09 >1)) {
    VarNamesExtra_m09 = cbind(Form = "MNH09", VarNamesExtra_m09)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m09 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m09)
  }
  
  if (length(VarNamesExtra_m09 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m09)
  }
}
#*****************************************************************************
#*MNH10
#*****************************************************************************
if (exists("mnh10")==TRUE){
  
  #*get variable list for MNH10 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH10") %>% select("VarName")
  VarNames_m10 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH10 from data  
  VarNames_data <- colnames(mnh10) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m10 <- as.data.frame(setdiff(VarNames_m10,VarNames_data))
  names(VarNamesMissing_m10) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m10 >1)) {
    VarNamesMissing_m10 = cbind(Form = "MNH10", VarNamesMissing_m10)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m10 <- as.data.frame(setdiff(VarNames_data,VarNames_m10))
  names(VarNamesExtra_m10) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m10 >1)) {
    VarNamesExtra_m10 = cbind(Form = "MNH10", VarNamesExtra_m10)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m10 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m10)
  }
  
  if (length(VarNamesExtra_m10 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m10)
  }
}
#*****************************************************************************
#*MNH11
#*****************************************************************************
if (exists("mnh11")==TRUE){
  
  #*get variable list for MNH11 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH11") %>% select("VarName")
  VarNames_m11 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH11 from data  
  VarNames_data <- colnames(mnh11) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m11 <- as.data.frame(setdiff(VarNames_m11,VarNames_data))
  names(VarNamesMissing_m11) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m11 >1)) {
    VarNamesMissing_m11 = cbind(Form = "MNH11", VarNamesMissing_m11)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m11 <- as.data.frame(setdiff(VarNames_data,VarNames_m11))
  names(VarNamesExtra_m11) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m11 >1)) {
    VarNamesExtra_m11 = cbind(Form = "MNH11", VarNamesExtra_m11)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m11 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m11)
  }
  
  if (length(VarNamesExtra_m11 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m11)
  }
  
}
#*****************************************************************************
#*MNH12
#*****************************************************************************
if (exists("mnh12")==TRUE){
  
  #*get variable list for MNH12 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH12") %>% select("VarName")
  VarNames_m12 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH12 from data  
  VarNames_data <- colnames(mnh12) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m12 <- as.data.frame(setdiff(VarNames_m12,VarNames_data))
  names(VarNamesMissing_m12) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m12 >1)) {
    VarNamesMissing_m12 = cbind(Form = "MNH12", VarNamesMissing_m12)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m12 <- as.data.frame(setdiff(VarNames_data,VarNames_m12))
  names(VarNamesExtra_m12) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m12 >1)) {
    VarNamesExtra_m12 = cbind(Form = "MNH12", VarNamesExtra_m12)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m12 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m12)
  }
  
  if (length(VarNamesExtra_m12 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m12)
  }
  
}

#*****************************************************************************
#*mnh13
#*****************************************************************************
if (exists("mnh13")==TRUE){
  
  #*get variable list for MNH13 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH13") %>% select("VarName")
  VarNames_m13 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH13 from data  
  VarNames_data <- colnames(mnh13) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m13 <- as.data.frame(setdiff(VarNames_m13,VarNames_data))
  names(VarNamesMissing_m13) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m13 >1)) {
    VarNamesMissing_m13 = cbind(Form = "MNH13", VarNamesMissing_m13)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m13 <- as.data.frame(setdiff(VarNames_data,VarNames_m13))
  names(VarNamesExtra_m13) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m13 >1)) {
    VarNamesExtra_m13 = cbind(Form = "MNH13", VarNamesExtra_m13)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m13 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m13)
  }
  
  if (length(VarNamesExtra_m13 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m13)
  }
  
}
#*****************************************************************************
#*MNH14
#*****************************************************************************
if (exists("mnh14")==TRUE){
  
  #*get variable list for MNH14 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH14") %>% select("VarName")
  VarNames_m14 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH14 from data  
  VarNames_data <- colnames(mnh14) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m14 <- as.data.frame(setdiff(VarNames_m14,VarNames_data))
  names(VarNamesMissing_m14) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m14 >1)) {
    VarNamesMissing_m14 = cbind(Form = "MNH14", VarNamesMissing_m14)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m14 <- as.data.frame(setdiff(VarNames_data,VarNames_m14))
  names(VarNamesExtra_m14) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m14 >1)) {
    VarNamesExtra_m14 = cbind(Form = "MNH14", VarNamesExtra_m14)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m14 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m14)
  }
  
  if (length(VarNamesExtra_m14 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m14)
  }
}
#*****************************************************************************
#*MNH15
#*****************************************************************************
if (exists("mnh15")==TRUE){
  
  #*get variable list for MNH15 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH15") %>% select("VarName")
  VarNames_m15 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH15 from data  
  VarNames_data <- colnames(mnh15) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m15 <- as.data.frame(setdiff(VarNames_m15,VarNames_data))
  names(VarNamesMissing_m15) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m15 >1)) {
    VarNamesMissing_m15 = cbind(Form = "MNH15", VarNamesMissing_m15)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m15 <- as.data.frame(setdiff(VarNames_data,VarNames_m15))
  names(VarNamesExtra_m15) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m15 >1)) {
    VarNamesExtra_m15 = cbind(Form = "MNH15", VarNamesExtra_m15)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m15 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m15)
  }
  
  if (length(VarNamesExtra_m15 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m15)
  }
  
}
#*****************************************************************************
#*MNH16
#*****************************************************************************
if (exists("mnh16")==TRUE){
  
  #*get variable list for MNH16 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH16") %>% select("VarName")
  VarNames_m16 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH16 from data  
  VarNames_data <- colnames(mnh16) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m16 <- as.data.frame(setdiff(VarNames_m16,VarNames_data))
  names(VarNamesMissing_m16) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m16 >1)) {
    VarNamesMissing_m16 = cbind(Form = "MNH16", VarNamesMissing_m16)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m16 <- as.data.frame(setdiff(VarNames_data,VarNames_m16))
  names(VarNamesExtra_m16) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m16 >1)) {
    VarNamesExtra_m16 = cbind(Form = "MNH16", VarNamesExtra_m16)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m16 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m16)
  }
  
  if (length(VarNamesExtra_m16 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m16)
  }
  
}
#*****************************************************************************
#*MNH17
#*****************************************************************************
if (exists("mnh17")==TRUE){
  
  #*get variable list for MNH17 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH17") %>% select("VarName")
  VarNames_m17 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH17 from data  
  VarNames_data <- colnames(mnh17) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m17 <- as.data.frame(setdiff(VarNames_m17,VarNames_data))
  names(VarNamesMissing_m17) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m17 >1)) {
    VarNamesMissing_m17 = cbind(Form = "MNH17", VarNamesMissing_m17)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m17 <- as.data.frame(setdiff(VarNames_data,VarNames_m17))
  names(VarNamesExtra_m17) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m17 >1)) {
    VarNamesExtra_m17 = cbind(Form = "MNH17", VarNamesExtra_m17)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m17 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m17)
  }
  
  if (length(VarNamesExtra_m17 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m17)
  }
}

#*****************************************************************************
#*MNH18
#*****************************************************************************
if (exists("mnh18")==TRUE){
  
  #*get variable list for MNH18 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH18") %>% select("VarName")
  VarNames_m18 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH18 from data  
  VarNames_data <- colnames(mnh18) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m18 <- as.data.frame(setdiff(VarNames_m18,VarNames_data))
  names(VarNamesMissing_m18) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m18 >1)) {
    VarNamesMissing_m18 = cbind(Form = "MNH18", VarNamesMissing_m18)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m18 <- as.data.frame(setdiff(VarNames_data,VarNames_m18))
  names(VarNamesExtra_m18) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m18 >1)) {
    VarNamesExtra_m18 = cbind(Form = "MNH18", VarNamesExtra_m18)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m18 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m18)
  }
  
  if (length(VarNamesExtra_m18 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m18)
  }
  
}
#*****************************************************************************
#*MNH19
#*****************************************************************************
if (exists("mnh19")==TRUE){
  
  #*get variable list for MNH19 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH19") %>% select("VarName")
  VarNames_m19 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH19 from data  
  VarNames_data <- colnames(mnh19) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m19 <- as.data.frame(setdiff(VarNames_m19,VarNames_data))
  names(VarNamesMissing_m19) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m19 >1)) {
    VarNamesMissing_m19 = cbind(Form = "MNH19", VarNamesMissing_m19)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m19 <- as.data.frame(setdiff(VarNames_data,VarNames_m19))
  names(VarNamesExtra_m19) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m19 >1)) {
    VarNamesExtra_m19 = cbind(Form = "MNH19", VarNamesExtra_m19)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m19 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m19)
  }
  
  if (length(VarNamesExtra_m19 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m19)
  }
  
}
#*****************************************************************************
#*MNH20
#*****************************************************************************
if (exists("mnh20")==TRUE){
  
  #*get variable list for MNH20 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH20") %>% select("VarName")
  VarNames_m20 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH20 from data  
  VarNames_data <- colnames(mnh20) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m20 <- as.data.frame(setdiff(VarNames_m20,VarNames_data))
  names(VarNamesMissing_m20) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m20 >1)) {
    VarNamesMissing_m20 = cbind(Form = "MNH20", VarNamesMissing_m20)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m20 <- as.data.frame(setdiff(VarNames_data,VarNames_m20))
  names(VarNamesExtra_m20) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m20 >1)) {
    VarNamesExtra_m20 = cbind(Form = "MNH20", VarNamesExtra_m20)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m20 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m20)
  }
  
  if (length(VarNamesExtra_m20 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m20)
  }
  
}
#*****************************************************************************
#*MNH21
#*****************************************************************************
if (exists("mnh21")==TRUE){
  
  #*get variable list for MNH21 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH21") %>% select("VarName")
  VarNames_m21 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH21 from data  
  VarNames_data <- colnames(mnh21) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m21 <- as.data.frame(setdiff(VarNames_m21,VarNames_data))
  names(VarNamesMissing_m21) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m21 >1)) {
    VarNamesMissing_m21 = cbind(Form = "MNH21", VarNamesMissing_m21)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m21 <- as.data.frame(setdiff(VarNames_data,VarNames_m21))
  names(VarNamesExtra_m21) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m21 >1)) {
    VarNamesExtra_m21 = cbind(Form = "MNH21", VarNamesExtra_m21)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m21 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m21)
  }
  
  if (length(VarNamesExtra_m21 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m21)
  }
}

#*****************************************************************************
#*MNH22
#*****************************************************************************
if (exists("mnh22")==TRUE){
  
  #*get variable list for MNH22 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH22") %>% select("VarName")
  VarNames_m22 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH22 from data  
  VarNames_data <- colnames(mnh22) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m22 <- as.data.frame(setdiff(VarNames_m22,VarNames_data))
  names(VarNamesMissing_m22) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m22 >1)) {
    VarNamesMissing_m22 = cbind(Form = "MNH22", VarNamesMissing_m22)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m22 <- as.data.frame(setdiff(VarNames_data,VarNames_m22))
  names(VarNamesExtra_m22) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m22 >1)) {
    VarNamesExtra_m22 = cbind(Form = "MNH22", VarNamesExtra_m22)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m22 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m22)
  }
  
  if (length(VarNamesExtra_m22 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m22)
  }
}
#*****************************************************************************
#*MNH23
#*****************************************************************************
if (exists("mnh23")==TRUE){
  
  #*get variable list for MNH23 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH23") %>% select("VarName")
  VarNames_m23 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH23 from data  
  VarNames_data <- colnames(mnh23) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m23 <- as.data.frame(setdiff(VarNames_m23,VarNames_data))
  names(VarNamesMissing_m23) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m23 >1)) {
    VarNamesMissing_m23 = cbind(Form = "MNH23", VarNamesMissing_m23)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m23 <- as.data.frame(setdiff(VarNames_data,VarNames_m23))
  names(VarNamesExtra_m23) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m23 >1)) {
    VarNamesExtra_m23 = cbind(Form = "MNH23", VarNamesExtra_m23)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m23 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m23)
  }
  
  if (length(VarNamesExtra_m23 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m23)
  }
}
#*****************************************************************************
#*MNH24
#*****************************************************************************
if (exists("mnh24")==TRUE){
  
  #*get variable list for MNH24 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH24") %>% select("VarName")
  VarNames_m24 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH24 from data  
  VarNames_data <- colnames(mnh24) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m24 <- as.data.frame(setdiff(VarNames_m24,VarNames_data))
  names(VarNamesMissing_m24) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m24 >1)) {
    VarNamesMissing_m24 = cbind(Form = "MNH24", VarNamesMissing_m24)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m24 <- as.data.frame(setdiff(VarNames_data,VarNames_m24))
  names(VarNamesExtra_m24) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m24 >1)) {
    VarNamesExtra_m24 = cbind(Form = "MNH24", VarNamesExtra_m24)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m24 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m24)
  }
  
  if (length(VarNamesExtra_m24 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m24)
  }
}

#*****************************************************************************
#*MNH25
#*****************************************************************************
if (exists("mnh25")==TRUE){
  
  #*get variable list for MNH25 from data dictionary 
  VarNames_form <- variable_names %>%  filter(Form == paste0("MNH25_", site))
  
  VarNames_m25 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH25 from data  
  VarNames_data <- colnames(mnh25) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m25 <- as.data.frame(setdiff(VarNames_m25,VarNames_data))
  names(VarNamesMissing_m25) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m25 >1)) {
    VarNamesMissing_m25 = cbind(Form = "MNH25", VarNamesMissing_m25)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m25 <- as.data.frame(setdiff(VarNames_data,VarNames_m25))
  names(VarNamesExtra_m25) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m25 >1)) {
    VarNamesExtra_m25 = cbind(Form = "MNH25", VarNamesExtra_m25)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m25 > 1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m25)
  }
  
  if (length(VarNamesExtra_m25 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m25)
  }
}

#*****************************************************************************
#*MNH26
#*****************************************************************************
if (exists("mnh26")==TRUE){
  
  #*get variable list for MNH26 from data dictionary 
  VarNames_form <- variable_names %>% filter(Form == "MNH26") %>% select("VarName")
  VarNames_m26 = as.vector(VarNames_form$VarName)
  
  #*get variable list for MNH26 from data  
  VarNames_data <- colnames(mnh26) 
  
  #*compare the variables that are in the data dictionary but are not in the data. These will be missing variables 
  VarNamesMissing_m26 <- as.data.frame(setdiff(VarNames_m26,VarNames_data))
  names(VarNamesMissing_m26) = "Missing Variables"
  
  # name data by form if missing exist 
  if (length(VarNamesMissing_m26 >1)) {
    VarNamesMissing_m26 = cbind(Form = "MNH26", VarNamesMissing_m26)
  }
  
  #*compare the variables that are in the data but not in the data dictionary. These will be extra variables
  VarNamesExtra_m26 <- as.data.frame(setdiff(VarNames_data,VarNames_m26))
  names(VarNamesExtra_m26) = "Extra Variables"
  
  # name data by form if extra exist 
  if (length(VarNamesExtra_m26 >1)) {
    VarNamesExtra_m26 = cbind(Form = "MNH26", VarNamesExtra_m26)
  }
  
  #*bind with other forms
  if (length(VarNamesMissing_m26 >1)){
    VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_m26)
  }
  
  if (length(VarNamesExtra_m26 >1)){
    VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_m26)
  }
}

#*****************************************************************************
#* Organize the data to match the query report template 
#*****************************************************************************
if (dim(VarNamesMissing)[1] > 1){
  
  # update naming 
  names(VarNamesMissing) = c("Form", "Variable Name")
  
  # remove first empty row 
  VarNamesMissing = VarNamesMissing[-1,]
  
  ## add additional columns 
  VarNamesMissing = cbind(QueryID = NA, 
                          UploadDate = UploadDate, 
                          ScrnID = "NA", MomID = "NA",
                          PregID = "NA", InfantID = "NA",
                          VisitType = "NA",
                          VisitDate = "NA", 
                          VarNamesMissing, 
                          `Variable Value` = "NA",
                          FieldType = "NA", 
                          EditType = "Missing Variable", 
                          DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  VarNamesMissing$Form_Edit_Type <- paste(VarNamesMissing$Form,"_",VarNamesMissing$EditType)
  
  ## assign queryid -- edit type id for missing variables is 03 
  VarNamesMissing <- VarNamesMissing %>% 
    mutate(QueryID = paste0(Form, "_", `Variable Name`, "_", "03")
    )
  
}

## assign queryid -- 
# edit type id for out of range is 05 
# edit type id for invalid response option is 09

if (dim(VarNamesExtra)[1] > 1){
  
  # update naming 
  names(VarNamesExtra) = c("Form", "Variable Name")
  
  # remove first empty row 
  VarNamesExtra = VarNamesExtra[-1,]
  
  
  ## add additional columns 
  VarNamesExtra = cbind(QueryID = NA, 
                        UploadDate = UploadDate, 
                        ScrnID = "NA", MomID = "NA",
                        PregID = "NA", InfantID = "NA",
                        VisitType = "NA",
                        VisitDate = "NA", 
                        VarNamesExtra, 
                        `Variable Value` = "NA",
                        FieldType = "NA", 
                        EditType = "Extra Variable", 
                        DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  VarNamesExtra$Form_Edit_Type <- paste(VarNamesExtra$Form,"_",VarNamesExtra$EditType)
  
  ## assign queryid -- edit type id for missing variables is 02 
  VarNamesExtra <- VarNamesExtra %>% 
    mutate(QueryID = paste0(Form, "_", `Variable Name`, "_", "02")
    )
  
}

# merge together 
missing_query <- rbind(VarNamesMissing, VarNamesExtra)

## export variable checking query 
save(missing_query, file = paste0(maindir, "/queries/missing_query.rda"))
