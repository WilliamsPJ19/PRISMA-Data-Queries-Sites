# PRiSMA_Data_Codes
## Description
This repository contains all PRiSMA codes uploaded by GWU team that will be shared and used by sites. We ask that each site please **create their own fork of this repository** and upload their code and output there. 

#### :pushpin: *Updated on 10/31 -- Updated PRiSMA1a_daily_duplicates_check_OCT302022.do to include all forms*

## File Structure
**1a\. `PRiSMA1a_daily_duplicates_check.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: OCT302022 
   - input: All MNH raw data
   - output: excel file with all duplicate screening IDs, mom IDs, and infant IDs
   - timing: daily  
   - function: 
     - Checks for duplicate IDs 
     - Check to make sure all women enrolled in study are were also screened
     #### :pushpin: *includes code for all forms*
     
**1b\. `PRiSMA1b_core_variables_check.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: OCT102022 
   - input: All MNH raw data
   - output: log file with missing core variables 
   - timing: daily  
   - function: 
     - Checks for missing core variables in the dataset 
     #### :pushpin: *includes code for all forms*

